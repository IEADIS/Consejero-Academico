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
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.springframework.stereotype.Service;

import org.rosuda.JRI.Rengine;
import org.rosuda.JRI.REXP;


/**
 *
 * @author Zekkenn
 */
@Service
public class RepositoryRImpl implements RepositoryR{

    private Map<String, String> nameFinalModels;
    private String path;
    private Rengine engine;
    private final String READMODEL = "model <- readRDS( '%s')";
    private final String PREDICTMODEL = "pred <- as.numeric(predict( model, data.frame( Grade1 = c(%f), Grade2 = c(%f) ) ) )";
    
    public RepositoryRImpl() {
        super();
        path = System.getProperty("user.dir").replace("/Servicio/DataAnalytics", "");
        path = path + "/Data/Models/Absolute";
        engine = Rengine.getMainEngine();
        if(engine == null){
            System.out.println ("LOADING R");
            engine=new Rengine (new String [] {"--vanilla"}, false, null);
            if (!engine.waitForR())
            {
                System.out.println ("Cannot load R");
            }
        }
    }
    
    private List<File> loadData(String subj){
        List<File> nameModels = new ArrayList<>();
        System.out.println(path);
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
        /*for ( File f : nameModels ){
            System.out.println(f.getAbsolutePath());
        }*/
        return nameModels;
    }
    
    @Override
    public Materia classifyStudent(String subj, double note1, double note2) throws PersistenceException {
        List<File> models = loadData(subj);
        String name = models.get( models.size()-1 ).getName();
        String year = name.replace("abs-" + subj + "_", "").replace(".rds", "");
        String pathModel = models.get( models.size()-1 ).getAbsolutePath().replace("\\", "/");
        
        engine.eval( String.format( READMODEL , pathModel) );
        engine.eval( String.format( PREDICTMODEL , note1, note2) );
        double res = engine.eval("pred").asDouble();
        //System.out.println( res );
        //System.out.println(String.format( READMODEL , pathModel));
        //System.out.println( String.format( PREDICTMODEL , note1, note2) );
        boolean pass = (res == 1)? true : ( res == 2 )? false : res >= 30;
        
        Materia response = new Materia(subj, note1, note2, 
                pass, year);
        return response;
    }
    
}
