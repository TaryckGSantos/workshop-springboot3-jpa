## Visão geral

Este projeto implementa uma API REST para um **e-commerce** simples, com:

* **Usuários (User)**
* **Pedidos (Order)** e seu **ciclo de vida** (OrderStatus)
* **Itens de pedido (OrderItem)** com **chave composta** (Order + Product)
* **Pagamentos (Payment)** (somente leitura, associado a Order)
* **Produtos (Product)** com **soft delete** via `active = true/false`
* **Categorias (Category)** com **soft delete** e **desassociação de produtos** ao ser desativada
* Camadas **Resource (Controller)** → **Service** → **Repository** → **Entity**

A API aceita objetos de domínio diretamente, com validações e proteções implementadas nos Services.

---

## Arquitetura em camadas

### 1) Resource (Controller)

* Responsável por **expor os endpoints REST**.
* Recebe requisições (JSON), chama o **Service** correspondente e devolve uma resposta HTTP (`ResponseEntity`).
* **Não contém lógica de negócio**.

### 2) Service

* Contém a **lógica de negócio** da aplicação.
* Usa `@Transactional` para garantir consistência nas operações.
* Faz validações, controla fluxos e acessa os **Repositories**.

### 3) Repository

* Camada de **acesso ao banco**.
* Extende `JpaRepository<Entity, ID>` para CRUD básico.
* Sem regras de negócio.

### 4) Entity

* Representa tabelas no banco de dados.
* Usa anotações JPA (`@Entity`, `@ManyToOne`, `@OneToMany`, `@ManyToMany`, etc.).
* Controla serialização JSON com `@JsonIgnore` e `@JsonFormat`.

---

## Fluxo de uma requisição típica

1. O **Resource** recebe o HTTP (ex.: `POST /orders` com JSON).
2. Monta os parâmetros e chama o **Service**.
3. O **Service** aplica regras, validações e chama o **Repository**.
4. A **Entity** é salva e retornada.
5. O **Resource** devolve `ResponseEntity` com status HTTP (200, 201, 204, etc.).

---

## Entidades (Domain Model)

### User

* Armazena informações básicas: `id`, `name`, `email`, `phone`, `password`.
* Relacionamento: `@OneToMany(mappedBy="client")` → lista de Orders.
* Usa `@JsonIgnore` para evitar loops na serialização.

### Order

* Representa um pedido.
* Campos: `id`, `moment`, `orderStatus`, `client`, `items`, `payment`.
* Sempre criado com 1 item obrigatório (productId + quantity).
* Tem método `getTotal()` para calcular o total do pedido.

### OrderStatus (Enum)

* Valores possíveis: `WAITING_PAYMENT`, `PAID`, `SHIPPED`, `DELIVERED`, `CANCELED`.
* Controla o estado do pedido.

### OrderItem

* Representa o item de um pedido.
* Chave composta (`OrderItemPK`) = (order, product).
* Guarda `quantity` e `price` do momento da compra.
* `getSubTotal()` calcula o subtotal.

### Payment

* Relacionamento 1:1 com `Order`.
* Representa pagamento associado ao pedido.

### Product

* Campos: `name`, `description`, `price`, `imgUrl`, `active`.
* **Soft delete** com `active=false`.
* Relacionamento N:N com `Category`.

### Category

* Campos: `id`, `name`, `active`.
* Relacionamento N:N com `Product`.
* No soft delete, remove a associação de todos os produtos (limpa join table).

---

## Repositórios

Exemplo padrão:

```
public interface ProductRepository extends JpaRepository<Product, Long> {}
```

Todos seguem esse padrão.

---

## Services — responsabilidades e diferenças

### ProductService

* Filtra apenas produtos ativos.
* `insert()` cria ativo e associa categorias existentes.
* `update()` modifica apenas produtos ativos.
* `delete()` faz soft delete (active=false).
* `restore()` reativa produto.
* `findAllInactive()` lista inativos.

### CategoryService

* Filtra apenas categorias ativas.
* `insert()` cria categoria ativa.
* `update()` altera nome.
* `delete()` faz soft delete **e desassocia dos produtos**.
* `restore()` reativa categoria.
* `findAllInactive()` lista inativas.

### OrderService

* `insertWithFirstItem()` cria pedido com item obrigatório (user + product + quantity).
* `updateStatus()` valida mudanças de status com regras rígidas.
* `findAll()` e `findById()` retornam pedidos com seus itens.

---

## Regras de negócio

### Pedidos (Order)

* Criado em estado `WAITING_PAYMENT`.
* Transições válidas:

  * WAITING_PAYMENT → PAID | CANCELED
  * PAID → SHIPPED | CANCELED
  * SHIPPED → DELIVERED
  * DELIVERED e CANCELED = estados finais.

### Produtos e Categorias

* `active=false` = item inativo.
* Produtos inativos não aparecem em listagens nem são atualizáveis.
* Categorias inativas são removidas de todos os produtos associados.

---

## Transações (`@Transactional`)

* Usado nos métodos que alteram dados (`insert`, `update`, `delete`, `restore`).
* Garante que, em caso de erro, **tudo é revertido** (rollback automático).

---

## Serialização JSON

* `@JsonIgnore` evita loops (ex.: `User.getOrders()`).
* `@JsonFormat` define formato de data ISO 8601.
* A API expõe entidades diretamente (sem DTOs), com validações nos Services.

---

## Endpoints e exemplos (Postman)

### **Product**

**Criar:**

```http
POST /products
{
  "name": "Notebook Gamer",
  "description": "RTX 4060, tela 144Hz",
  "price": 8999.90,
  "imgUrl": "https://meusite.com/img/notebook.jpg",
  "categories": [ { "id": 1 } ]
}
```

**Atualizar:**

```http
PUT /products/5
{
  "name": "Notebook Gamer PRO",
  "description": "RTX 4070",
  "price": 10999.90,
  "imgUrl": "https://meusite.com/img/notebook-pro.jpg"
}
```

**Soft delete:**

```http
DELETE /products/5
```

**Restaurar:**

```http
PATCH /products/5/restore
```

**Listar inativos:**

```http
GET /products/inactive
```

---

### **Category**

**Criar:**

```http
POST /categories
{
  "name": "Gamer"
}
```

**Atualizar:**

```http
PUT /categories/3
{
  "name": "Acessórios Gamer"
}
```

**Soft delete (desassocia produtos):**

```http
DELETE /categories/3
```

**Restaurar:**

```http
PATCH /categories/3/restore
```

**Listar inativas:**

```http
GET /categories/inactive
```

---

### **Order**

**Criar pedido com item:**

```http
POST /orders
{
  "clientId": 1,
  "productId": 3,
  "quantity": 2
}
```

**Atualizar status:**

```http
PATCH /orders/10/status/PAID
```

**Listar pedidos:**

```http
GET /orders
```

---

## Tratamento de erros

* **404 Not Found:** entidade inexistente ou inativa.
* **400 Bad Request:** parâmetros inválidos.
* **IllegalStateException:** mudança de status inválida.

---

## Decisões de design

* Sem DTOs: simplifica, mantém lógica no Service.
* Soft delete: mantém histórico e evita inconsistência.
* Order controlado: o servidor decide status e momento.
* Category limpa relacionamentos ao ser desativada.
* OrderItem com preço congelado e chave composta.

---

## Como rodar o projeto

1. Tenha **Java 17+** e **Maven**.
2. Configure `application.properties` (H2 ou MySQL).
3. Execute `mvn spring-boot:run`.
4. Teste endpoints no **Postman**.

---
