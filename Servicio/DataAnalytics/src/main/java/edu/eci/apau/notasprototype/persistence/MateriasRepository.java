/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence;

import java.util.List;
/**
 *
 * @author Esteban
 */
public interface MateriasRepository{
    
    public List<String> loadAllMaterias();
}
