/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.eci.apau.notasprototype.controllers;

import edu.eci.apau.notasprototype.model.Materia;
import edu.eci.apau.notasprototype.services.MateriaServices;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author Esteban
 * @author Sebastian
 */
@Controller
@RestController
@RequestMapping("/evaluate")
public class NotasAPIController {

  @Autowired MateriaServices materiaServices;

  @GetMapping("/currentAsignatures")
  public List<String> getCurrentAsignatures() {
    return materiaServices.getCurrentMaterias();
  }

  @GetMapping("/classifyStudent/{subj}")
  public ResponseEntity<?> getClassificationCancelSubject(
      @PathVariable String subj,
      @RequestParam("grade1") double note1,
      @RequestParam("grade2") double note2) {
    try {
      return new ResponseEntity<>(
          materiaServices.classifyStudent(subj, note1, note2), HttpStatus.ACCEPTED);
    } catch (Exception e) {
      e.printStackTrace();
      return new ResponseEntity<>(e.getLocalizedMessage(), HttpStatus.NOT_FOUND);
    }
  }

  @PostMapping("/estimate")
  public ResponseEntity<?> makeEstimateAsignature(@RequestBody Materia materia) {
    try {
      return new ResponseEntity<>(materiaServices.getEstimate(materia), HttpStatus.CREATED);
    } catch (Exception e) {
      return new ResponseEntity<>(e.getLocalizedMessage(), HttpStatus.FORBIDDEN);
    }
  }
}
