/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.persistence.filesPersistence;

import edu.eci.apau.notasprototype.model.Materia;
import edu.eci.apau.notasprototype.persistence.PersistenceException;
import edu.eci.apau.notasprototype.persistence.RepositoryR;
import org.rosuda.JRI.Rengine;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/** @author Zekkenn */
@Service
public class RepositoryRImpl implements RepositoryR {

  private final String READMODEL = "model <- readRDS( '%s')";
  private final String PREDICTMODEL =
      "pred <- as.numeric(predict( model, data.frame( Grade1 = c(%d), Grade2 = c(%d) ) ) )";
  private Map<String, String> nameFinalModels;
  private String path;
  private Rengine engine;

  public RepositoryRImpl() {
    super();
    path = System.getProperty("user.dir").replace("\\Servicio\\DataAnalytics", "");
    path = path + "\\Data\\Models\\Absolute";
    engine = Rengine.getMainEngine();
    if (engine == null) {
      System.out.println("LOADING R");
      engine = new Rengine(new String[] {"--vanilla"}, false, null);
      if (!engine.waitForR()) {
        System.out.println("Cannot load R");
      }
    }
  }

  private List<File> loadData(String subj) {
    List<File> nameModels = new ArrayList<>();
    final File dir = new File(path);
    for (File f : dir.listFiles()) {
      if (!f.isDirectory()
          && f.isFile()
          && f.getName().toLowerCase().trim().contains(subj.toLowerCase().trim())) {
        nameModels.add(f);
      }
    }
    Collections.sort(nameModels, (s1, s2) -> s1.getName().compareToIgnoreCase(s2.getName()));
    return nameModels;
  }

  @Override
  public Materia classifyStudent(String subj, Double note1, Double note2)
      throws PersistenceException {
    List<File> models = loadData(subj);
    String name = models.get(models.size() - 1).getName();
    String year = name.replace("abs-" + subj + "_", "").replace(".rds", "");
    String pathModel = models.get(models.size() - 1).getAbsolutePath().replace("\\", "/");

    engine.eval("pred <- 0");
    engine.eval("model <- NULL");
    engine.eval(String.format(READMODEL, pathModel));
    engine.eval(String.format(PREDICTMODEL, note1.intValue(), note2.intValue()));

    double res = engine.eval("pred").asDouble();
    boolean pass = (res == 1) || (res != 2) && res >= 30;

    Materia response =
        Materia.builder()
            .nombre(subj)
            .nota1(note1)
            .nota2(note2)
            .classification(pass)
            .lastYearTrained(year)
            .build();
    return response;
  }
}
