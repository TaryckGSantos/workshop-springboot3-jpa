package com.educandoweb.course.resources;

import java.net.URI;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import com.educandoweb.course.entities.Order;
import com.educandoweb.course.services.OrderService;

@RestController
@RequestMapping(value = "/orders")
public class OrderResource {

	@Autowired
	private OrderService service;
	
	public static class CreateOrderRequest {
        public Long clientId;
        public Long productId;
        public Integer quantity;
    }

	@GetMapping // Indica que o método responde a requisição do tipo get do HTTP
	public ResponseEntity<List<Order>> findAll() {
		List<Order> list = service.findAll();
		return ResponseEntity.ok().body(list); // ok para retornar a resposta com sucesso no HTTP e body para retornar o
												// corpo da resposta
	}

	@GetMapping(value = "/{id}")
	public ResponseEntity<Order> findById(@PathVariable Long id) {
		Order obj = service.findById(id);
		return ResponseEntity.ok().body(obj);
	}

	@PostMapping
	public ResponseEntity<Order> insert(@RequestBody CreateOrderRequest request) {
	    Order obj = service.insertWithFirstItem(request.clientId, request.productId, request.quantity);
	    URI uri = ServletUriComponentsBuilder.fromCurrentRequest()
	        .path("/{id}").buildAndExpand(obj.getId()).toUri();
	    return ResponseEntity.created(uri).body(obj);
	}

	@PatchMapping("/{id}/status/{status}")
	public ResponseEntity<Order> updateStatus(@PathVariable Long id, @PathVariable String status) {
		return ResponseEntity.ok(service.updateStatus(id, status));
	}
}
