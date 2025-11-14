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
	private ProductRepository productRepository;

	@Autowired
	private CategoryRepository categoryRepository;

	// Lista apenas ativos
	public List<Product> findAll() {
		return productRepository.findAll().stream().filter(p -> Boolean.TRUE.equals(p.getActive())).toList();
	}

	// Busca apenas ativos
	public Product findById(Long id) {
		Product p = productRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (!Boolean.TRUE.equals(p.getActive())) {
			throw new ResourceNotFoundException(id);
		}
		return p;
	}

    @Transactional
	public Product insert(Product obj) {
		obj.setActive(true);

		Set<Category> receivedCategories = new HashSet<>(obj.getCategories());
	    obj.getCategories().clear();

	    for (Category cat : receivedCategories) {
	        Category existing = categoryRepository.findById(cat.getId())
	                .orElseThrow(() -> new ResourceNotFoundException(cat.getId()));
	        obj.getCategories().add(existing);
	    }
	    
		return productRepository.save(obj);
	}

    @Transactional
	public Product update(Long id, Product obj) {
		Product entity = findById(id);
		entity.setName(obj.getName());
		entity.setDescription(obj.getDescription());
		entity.setPrice(obj.getPrice());
		entity.setImgUrl(obj.getImgUrl());
		return productRepository.save(entity);
	}

	// Marca como inativo
	@Transactional
	public void delete(Long id) {
		Product entity = productRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		if (Boolean.FALSE.equals(entity.getActive())) {
            return; // já inativo
        }
		entity.setActive(false);
		productRepository.save(entity);
	}

	// Restaura
	@Transactional
	public Product restore(Long id) {
		Product entity = productRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(id));
		entity.setActive(true);
		return productRepository.save(entity);
	}

	// Lista inativos
	public List<Product> findAllInactive() {
		return productRepository.findAll().stream().filter(p -> Boolean.FALSE.equals(p.getActive())).toList();
	}
}
