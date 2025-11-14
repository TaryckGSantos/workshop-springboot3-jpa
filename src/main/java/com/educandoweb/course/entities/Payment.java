package com.educandoweb.course.entities;

import java.io.Serializable;
import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonIgnore;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.MapsId;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.EqualsAndHashCode.Include;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "tb_payment")
@Getter
@Setter
@NoArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Payment implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Id
	@Include
	private Long id;
	
	private Instant moment;
	
	@JsonIgnore
	@OneToOne
	@MapsId
	private Order order;

    public Payment(Instant moment, Order order) {
        this.moment = moment;
        this.order = order; // ao setar o order, o id do Payment vira o id do Order
    }
}
