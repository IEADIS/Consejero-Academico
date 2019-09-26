/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.services;

import edu.eci.apau.notasprototype.model.Estimate;
import edu.eci.apau.notasprototype.model.Materia;
import edu.eci.apau.notasprototype.persistence.EstimatesRepository;
import edu.eci.apau.notasprototype.persistence.MateriasRepository;
import edu.eci.apau.notasprototype.persistence.PersistenceException;
import edu.eci.apau.notasprototype.persistence.RepositoryR;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/** @author Esteban */
@Service
public class MateriaServices implements Services {

  @Autowired private MateriasRepository materiasRepository;
  @Autowired private EstimatesRepository estimatesRepository;
  @Autowired private RepositoryR rRepository;

  @Override
  public List<String> getCurrentMaterias() {
    return materiasRepository.loadAllMaterias();
  }

  @Override
  public Estimate getEstimate(Materia materia) {
    return estimatesRepository.loadEstimate(
        materia.getNombre(), materia.getNota1(), materia.getNota2());
  }

  @Override
  public Materia classifyStudent(String subj, double note1, double note2)
      throws PersistenceException {
    return rRepository.classifyStudent(subj, note1, note2);
  }
}
