package com.educandoweb.course.services;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.educandoweb.course.entities.Order;
import com.educandoweb.course.entities.OrderItem;
import com.educandoweb.course.entities.Product;
import com.educandoweb.course.entities.User;
import com.educandoweb.course.entities.enums.OrderStatus;
import com.educandoweb.course.repositories.OrderItemRepository;
import com.educandoweb.course.repositories.OrderRepository;
import com.educandoweb.course.repositories.ProductRepository;
import com.educandoweb.course.repositories.UserRepository;
import com.educandoweb.course.services.exceptions.ResourceNotFoundException;

import jakarta.persistence.EntityNotFoundException;

@Service
public class OrderService {

	@Autowired
	private OrderRepository repository;

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private ProductRepository productRepository;

	@Autowired
	private OrderItemRepository orderItemRepository;

	public List<Order> findAll() {
		return repository.findAll();
	}

	public Order findById(Long id) {
		Optional<Order> obj = repository.findById(id);
		return obj.get();
	}

	@Transactional
	public Order insertWithFirstItem(Long clientId, Long productId, Integer quantity) {
		if (clientId == null)
			throw new IllegalArgumentException("clientId é obrigatório.");
		validateItemParams(productId, quantity);

		User client = userRepository.findById(clientId).orElseThrow(() -> new ResourceNotFoundException(clientId));

		Order order = new Order();
		order.setClient(client);
		order.setMoment(Instant.now());
		order.setOrderStatus(OrderStatus.WAITING_PAYMENT);
		order = repository.save(order);

		addItemInternal(order, productId, quantity);

		return order;
	}

	@Transactional
	public Order updateStatus(Long id, String statusStr) {
		try {
			Order entity = repository.getReferenceById(id);
			OrderStatus newStatus = parseStatus(statusStr);
			validateTransition(entity.getOrderStatus(), newStatus);
			entity.setOrderStatus(newStatus);
			return repository.save(entity);
		} catch (EntityNotFoundException e) {
			throw new ResourceNotFoundException(id);
		}
	}
	
	private void validateItemParams(Long productId, Integer quantity) {
		if (productId == null)
			throw new IllegalArgumentException("productId é obrigatório.");
		if (quantity == null || quantity <= 0)
			throw new IllegalArgumentException("quantity deve ser > 0.");
	}

	// cria ou incrementa item do mesmo produto
	private void addItemInternal(Order order, Long productId, Integer quantity) {
		Product product = productRepository.findById(productId)
				.orElseThrow(() -> new ResourceNotFoundException(productId));

		OrderItem existing = order.getItems().stream().filter(it -> it.getProduct().getId().equals(productId))
				.findFirst().orElse(null);

		if (existing != null) {
			// se já existe, só soma a quantidade
			existing.setQuantity(existing.getQuantity() + quantity);
			orderItemRepository.save(existing);
		} else {
			// congela o preço no momento da compra
			Double priceAtOrder = product.getPrice();
			OrderItem oi = new OrderItem(order, product, quantity, priceAtOrder);
			order.getItems().add(oi);
			orderItemRepository.save(oi);
		}
	}

	private OrderStatus parseStatus(String statusStr) {
		if (statusStr == null)
			throw new IllegalArgumentException("Status obrigatório.");
		try {
			int code = Integer.parseInt(statusStr);
			return OrderStatus.valueOf(code);
		} catch (NumberFormatException e) {
			try {
				return OrderStatus.valueOf(statusStr.trim().toUpperCase());
			} catch (IllegalArgumentException ex) {
				throw new IllegalArgumentException("Status inválido: " + statusStr);
			}
		}
	}

	private void validateTransition(OrderStatus from, OrderStatus to) {
		if (from == OrderStatus.WAITING_PAYMENT) {
			if (!(to == OrderStatus.PAID || to == OrderStatus.CANCELED))
				throw new IllegalStateException("Transição inválida: " + from + " -> " + to);
		} else if (from == OrderStatus.PAID) {
			if (!(to == OrderStatus.SHIPPED || to == OrderStatus.CANCELED))
				throw new IllegalStateException("Transição inválida: " + from + " -> " + to);
		} else if (from == OrderStatus.SHIPPED) {
			if (to != OrderStatus.DELIVERED)
				throw new IllegalStateException("Transição inválida: " + from + " -> " + to);
		} else {
			throw new IllegalStateException("Pedido em estado final (" + from + ") não pode mudar.");
		}
	}
}
