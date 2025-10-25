package com.educandoweb.course.services;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.educandoweb.course.entities.Category;
import com.educandoweb.course.entities.Product;
import com.educandoweb.course.repositories.CategoryRepository;
import com.educandoweb.course.repositories.ProductRepository;
import com.educandoweb.course.services.exceptions.ResourceNotFoundException;

import jakarta.transaction.Transactional;

@Service
public class ProductService {

	@Autowired
	private ProductRepository repository;

	@Autowired
	private CategoryRepository categoryRepository;

	// Lista apenas ativos
	public List<Product> findAll() {
		return repository.findAll().stream().filter(p -> Boolean.TRUE.equals(p.getActive())).toList();
	}

	// Busca apenas ativos
	public Product findById(Long id) {
		Product p = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (!Boolean.TRUE.equals(p.getActive())) {
			throw new ResourceNotFoundException(id);
		}
		return p;
	}

	public Product insert(Product obj) {
		obj.setActive(true);

		Set<Category> receivedCategories = new HashSet<>(obj.getCategories());
	    obj.getCategories().clear();

	    for (Category cat : receivedCategories) {
	        Category existing = categoryRepository.findById(cat.getId())
	                .orElseThrow(() -> new ResourceNotFoundException(cat.getId()));
	        obj.getCategories().add(existing);
	    }
	    
		return repository.save(obj);
	}

	public Product update(Long id, Product obj) {
		Product entity = findById(id);
		entity.setName(obj.getName());
		entity.setDescription(obj.getDescription());
		entity.setPrice(obj.getPrice());
		entity.setImgUrl(obj.getImgUrl());
		return repository.save(entity);
	}

	// Marca como inativo
	@Transactional
	public void delete(Long id) {
		Product entity = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (Boolean.FALSE.equals(entity.getActive()))
			return; // já inativo
		entity.setActive(false);
		repository.save(entity);
	}

	// Restaura
	@Transactional
	public Product restore(Long id) {
		Product entity = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		entity.setActive(true);
		return repository.save(entity);
	}

	// Lista inativos
	public List<Product> findAllInactive() {
		return repository.findAll().stream().filter(p -> Boolean.FALSE.equals(p.getActive())).toList();
	}
}
