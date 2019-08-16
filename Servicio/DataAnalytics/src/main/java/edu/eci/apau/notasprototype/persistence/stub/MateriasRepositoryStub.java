/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence.stub;

import edu.eci.apau.notasprototype.persistence.MateriasRepository;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
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
    
    private String path;

    public MateriasRepositoryStub() {
        super();
        path = System.getProperty("user.dir").replace("/Servicio/DataAnalytics", "");
        path = path + "/Data/Models/Absolute";
    }
    
    @Override
    public List<String> loadAllMaterias() {
        
        Set<String> nameModels = new TreeSet<>();
        final File dir = new File(path);
        for ( File f : dir.listFiles() ){
            if ( !f.isDirectory() && f.isFile() ) {
                String temp = f.getName().replace("abs-", "").replace(".rds", "");
                nameModels.add(temp.substring( 0, temp.indexOf("_") ));
            }
        }
        /**Collections.sort(nameModels, new Comparator<String>() {
            @Override
            public int compare(String s1, String s2) { 
                return s1.compareToIgnoreCase(s2);
            }
        });*/
        /*for ( File f : nameModels ){
            System.out.println(f.getAbsolutePath());
        }*/
        
        return new ArrayList(nameModels);
    }
    
}
