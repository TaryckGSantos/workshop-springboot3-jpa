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
	private CategoryRepository categoryRepository;

	@Autowired
	private ProductRepository productRepository;

	public List<Category> findAll() {
		return categoryRepository.findAll().stream().filter(c -> Boolean.TRUE.equals(c.getActive())).toList();
	}

	public Category findById(Long id) {
		Category c = categoryRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (!Boolean.TRUE.equals(c.getActive())) {
			throw new ResourceNotFoundException(id);
		}
		return c;
	}

	@Transactional
	public Category insert(Category obj) {
		obj.setActive(true);
		return categoryRepository.save(obj);
	}

	@Transactional
	public Category update(Long id, Category obj) {
		Category entity = findById(id);
		entity.setName(obj.getName());
		return categoryRepository.save(entity);
	}

	@Transactional
	public void delete(Long id) {
		Category entity = categoryRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (Boolean.FALSE.equals(entity.getActive()))
			return; 

		Set<Product> linked = new HashSet<>(entity.getProducts());
		for (Product p : linked) {
			p.getCategories().remove(entity);
		}
		productRepository.saveAll(linked);

		entity.setActive(false);
		categoryRepository.save(entity);
	}

	// Restaurar (não reanexa automaticamente aos produtos)
	@Transactional
	public Category restore(Long id) {
		Category entity = categoryRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		entity.setActive(true);
		return categoryRepository.save(entity);
	}

	public List<Category> findAllInactive() {
		return categoryRepository.findAll().stream().filter(c -> Boolean.FALSE.equals(c.getActive())).toList();
	}
}
