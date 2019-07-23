/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence.stub;

import edu.eci.apau.notasprototype.persistence.MateriasRepository;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import org.springframework.stereotype.Service;

/**
 *
 * @author Esteban
 */
@Service
public class MateriasRepositoryStub implements MateriasRepository{
    
    //change to a thread-safe collection if a modification operation is included
    private static final List<String> MATERIAS=new LinkedList<>(Arrays.asList(new String[]{
            "PIMB",
            "PIMO",
            "MBDA",
            "POOB"
        }));
    
    @Override
    public List<String> loadAllMaterias() {
        return MATERIAS;
    }
    
}
