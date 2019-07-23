/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence.stub;

import edu.eci.apau.notasprototype.model.Estimate;
import edu.eci.apau.notasprototype.persistence.EstimatesRepository;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Service;

/**
 *
 * @author Esteban
 */
@Service
public class EstimatesRepositoryStub implements EstimatesRepository{

    //change to a thread-safe collection if a modification operation is included
    private static final Map<String, Estimate> MATERIAS;
    static
    {
        MATERIAS = new HashMap<String, Estimate>();
        MATERIAS.put("PIMB", new Estimate(0.24345,0.78892,-5.06264,0.7485));
        MATERIAS.put("PIMO", new Estimate(0.32474,0.78262,-5.00151,0.6851));
        MATERIAS.put("MBDA", new Estimate(0.02132,0.93097,3.30240,0.7319));
        MATERIAS.put("POOB", new Estimate(0.08688,0.81807,3.86584,0.6152));
    }
    
    @Override
    public Estimate loadEstimate(String materia, double nota1, double nota2) {
        MATERIAS.get(materia).makeEstimate(nota1, nota2);
        return MATERIAS.get(materia);
    }
    
}
