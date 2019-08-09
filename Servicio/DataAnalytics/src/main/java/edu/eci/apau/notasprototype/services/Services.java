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

/**
 *
 * @author sistemas
 */
public interface Services {
    
    public List<String> getCurrentMaterias() throws PersistenceException;
    
    public Estimate getEstimate(Materia materia) throws PersistenceException;
    
    public boolean classifyStudent( Materia materia ) throws PersistenceException;
    
}
