/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
/**
 *
 * @author Esteban
 */
@SpringBootApplication
@ComponentScan(basePackages = {"edu.eci.apau.notasprototype"})
public class NotasPrototypeAPI {

    public static void main(String[] args) {        
        SpringApplication.run(NotasPrototypeAPI.class, args);
    }
    
}
