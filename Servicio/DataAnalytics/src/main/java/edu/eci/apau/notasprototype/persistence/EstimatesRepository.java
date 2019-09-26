/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence;

import edu.eci.apau.notasprototype.model.Estimate;

/** @author Esteban */
public interface EstimatesRepository {

  Estimate loadEstimate(String materia, double nota1, double nota2);
}
