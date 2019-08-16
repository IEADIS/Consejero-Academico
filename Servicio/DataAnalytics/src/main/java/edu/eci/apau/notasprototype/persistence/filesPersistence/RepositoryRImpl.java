/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence.filesPersistence;

import edu.eci.apau.notasprototype.model.Materia;
import edu.eci.apau.notasprototype.persistence.PersistenceException;
import edu.eci.apau.notasprototype.persistence.RepositoryR;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.springframework.stereotype.Service;

import javax.script.*;
import org.renjin.script.*;
/**
 *
 * @author Zekkenn
 */
@Service
public class RepositoryRImpl implements RepositoryR{

    private Map<String, String> nameFinalModels;
    private RenjinScriptEngineFactory factory;
    private ScriptEngine engine;
    private String path;
    
    public RepositoryRImpl() {
        super();
        factory = new RenjinScriptEngineFactory();
        engine = factory.getScriptEngine();
        path = System.getProperty("user.dir").replace("\\Servicio\\DataAnalytics", "");
        path = path + "\\Data\\Absolute";
    }
    
    private List<File> loadData(String subj){
        List<File> nameModels = new ArrayList<>();
        final File dir = new File(path);
        for ( File f : dir.listFiles() ){
            if ( !f.isDirectory() && f.isFile() && f.getName().toLowerCase().trim().contains( subj.toLowerCase().trim() ) ) {
                nameModels.add(f);
            }
        }
        Collections.sort(nameModels, new Comparator<File>() {
            @Override
            public int compare(File s1, File s2) { 
                return s1.getName().compareToIgnoreCase(s2.getName());
            }
        });
        for ( File f : nameModels ){
            System.out.println(f.getAbsolutePath());
        }
        return nameModels;
    }
    
    @Override
    public Materia classifyStudent(String subj, double note1, double note2) throws PersistenceException {
        List<File> models = loadData(subj);
        String name = models.get( models.size()-1 ).getName();
        String year = name.replace("abs-" + subj + "_", "").replace(".rds", "");
        
        try {
            System.out.println("LOL =======================");
            engine.eval("library(installr)");
            engine.eval("updateR()");
            engine.eval( "model <- readRDS( '" + models.get( models.size()-1 ).getAbsolutePath().replace("\\", "/") + "')" );
            System.out.println("LOL =======================");
        } catch (ScriptException ex) {
            ex.printStackTrace();
            Logger.getLogger(RepositoryRImpl.class.getName()).log(Level.SEVERE, null, ex);
            throw new PersistenceException("Failed to Execute R Code");
        }
        
        Materia response = new Materia(subj, note1, note2, 
                true, year);
        return response;
    }
    
}
