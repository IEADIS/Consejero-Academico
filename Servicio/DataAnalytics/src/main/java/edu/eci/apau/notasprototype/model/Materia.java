/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.model;

/** @author Esteban */
public class Materia {

  private String nombre;
  private double nota1;
  private double nota2;
  private boolean classification;
  private String lastYearTrained;

  public Materia() {}

  public Materia(String nombre, double nota1, double nota2, boolean classification) {
    this.nombre = nombre;
    this.nota1 = nota1;
    this.nota2 = nota2;
    this.classification = classification;
  }

  public Materia(
      String nombre, double nota1, double nota2, boolean classification, String lastYearTrained) {
    this.nombre = nombre;
    this.nota1 = nota1;
    this.nota2 = nota2;
    this.classification = classification;
    this.lastYearTrained = lastYearTrained;
  }

  public String getLastYearTrained() {
    return lastYearTrained;
  }

  public void setLastYearTrained(String lastYearTrained) {
    this.lastYearTrained = lastYearTrained;
  }

  public String getNombre() {
    return nombre;
  }

  public void setNombre(String nombre) {
    this.nombre = nombre;
  }

  public double getNota1() {
    return nota1;
  }

  public void setNota1(double nota1) {
    this.nota1 = nota1;
  }

  public double getNota2() {
    return nota2;
  }

  public void setNota2(double nota2) {
    this.nota2 = nota2;
  }

  public boolean isClassification() {
    return classification;
  }

  public void setClassification(boolean classification) {
    this.classification = classification;
  }
}
