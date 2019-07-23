/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.model;

import java.util.function.Function;

/**
 *
 * @author Esteban
 */
public class Estimate {

    private double rSquared;
    private String decision;
    private double nota3;
    
    private final double nota1coef;
    private final double nota2coef;
    private final double intercept;
    
    public Estimate(double nota1coef, double nota2coef, double intercept, double rSquared) {
        this.rSquared = rSquared;
        this.nota1coef = nota1coef;
        this.nota2coef = nota2coef;
        this.intercept = intercept;
    }
    
    public void makeEstimate(double nota1, double nota2){
        this.nota3 = (nota1*nota1coef)+(nota2*nota2coef)+intercept;
        this.decision = (((nota3+nota2+nota1)/3D) < 30) ? "Cancele" : "NoCancele";
    }

    public double getrSquared() {
        return rSquared;
    }

    public void setrSquared(double rSquared) {
        this.rSquared = rSquared;
    }

    public String getDecision() {
        return decision;
    }

    public void setDecision(String decision) {
        this.decision = decision;
    }

    public double getNota3() {
        return nota3;
    }

    public void setNota3(double nota3) {
        this.nota3 = nota3;
    }
    
    
}
