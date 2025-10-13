package com.educandoweb.course.resources;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.educandoweb.course.entities.User;

@RestController
@RequestMapping(value = "/users")
public class UserResource {
	
	@GetMapping //Indica que o método responde a requisição do tipo get do HTTP
	public ResponseEntity<User> findAll(){
		User u = new User(1L, "Maria Pink", "maria@gmail.com", "999999", "12345");
		return ResponseEntity.ok().body(u); // ok para retornar a resposta com sucesso no HTTP e body para retornar o corpo da resposta
	}
}
