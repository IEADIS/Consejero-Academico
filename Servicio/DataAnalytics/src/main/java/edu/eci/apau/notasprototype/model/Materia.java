/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/** @author Esteban */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Materia {

  private String nombre;
  private double nota1;
  private double nota2;
  private boolean classification;
  private String lastYearTrained;
  private LocalDateTime localDateTime;

  public Materia(String nombre, double nota1, double nota2, boolean classification) {
    this.nombre = nombre;
    this.nota1 = nota1;
    this.nota2 = nota2;
    this.classification = classification;
  }
}
