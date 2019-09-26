/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.services;

import edu.eci.apau.notasprototype.model.Estimate;
import edu.eci.apau.notasprototype.model.Materia;
import edu.eci.apau.notasprototype.persistence.PersistenceException;

import java.util.List;

/** @author sistemas */
public interface Services {

  List<String> getCurrentMaterias() throws PersistenceException;

  Estimate getEstimate(Materia materia) throws PersistenceException;

  Materia classifyStudent(String subj, double note1, double note2)
      throws PersistenceException;
}
