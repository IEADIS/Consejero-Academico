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
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 *
 * @author Esteban
 */
@Service
public class MateriaServices {
    
    @Autowired
    MateriasRepository materiasRepository;
    
    @Autowired
    EstimatesRepository estimatesRepository;
    
    public List<String> getCurrentMaterias(){
        return materiasRepository.loadAllMaterias();
    }
    
    public Estimate getEstimate(Materia materia){
        return estimatesRepository.loadEstimate(materia.getNombre(), materia.getNota1(), materia.getNota2());
    }
}
