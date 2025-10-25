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
public class CategoryService {

	@Autowired
	private CategoryRepository repository;

	@Autowired
	private ProductRepository productRepository;

	public List<Category> findAll() {
		return repository.findAll().stream().filter(c -> Boolean.TRUE.equals(c.getActive())).toList();
	}

	public Category findById(Long id) {
		Category c = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (!Boolean.TRUE.equals(c.getActive())) {
			throw new ResourceNotFoundException(id);
		}
		return c;
	}

	@Transactional
	public Category insert(Category obj) {
		obj.setActive(true);
		return repository.save(obj);
	}

	@Transactional
	public Category update(Long id, Category obj) {
		Category entity = findById(id);
		entity.setName(obj.getName());
		return repository.save(entity);
	}

	@Transactional
	public void delete(Long id) {
		Category entity = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (Boolean.FALSE.equals(entity.getActive()))
			return; 

		Set<Product> linked = new HashSet<>(entity.getProducts());
		for (Product p : linked) {
			p.getCategories().remove(entity);
		}
		productRepository.saveAll(linked);

		entity.setActive(false);
		repository.save(entity);
	}

	// Restaurar (não reanexa automaticamente aos produtos)
	@Transactional
	public Category restore(Long id) {
		Category entity = repository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		entity.setActive(true);
		return repository.save(entity);
	}

	public List<Category> findAllInactive() {
		return repository.findAll().stream().filter(c -> Boolean.FALSE.equals(c.getActive())).toList();
	}
}
