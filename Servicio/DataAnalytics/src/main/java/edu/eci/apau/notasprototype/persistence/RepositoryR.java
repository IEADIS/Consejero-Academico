/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence;

import edu.eci.apau.notasprototype.model.Materia;

/**
 *
 * @author sistemas
 */
public interface RepositoryR {
    
    public boolean classifyStudent( Materia materia ) throws PersistenceException;
    
}
