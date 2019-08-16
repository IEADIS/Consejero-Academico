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
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 *
 * @author Esteban
 */

@Controller
@RestController
@RequestMapping("/evaluate")
public class NotasAPIController {
    
    @Autowired
    MateriaServices materiaServices;
//    @RequestMapping(value = "/index" ,method = RequestMethod.GET)
//    /*public ModelAndView getHome(ModelAndView modelAndView, Materia materia){
//        modelAndView.addObject("materia", materia);
//        modelAndView.setViewName("index");
//        return modelAndView;
//    }*/
//    public ResponseEntity<?> getHome(){
//        return new ResponseEntity<>("Home", HttpStatus.ACCEPTED);
//    }
    
    @RequestMapping(path = "/currentAsignatures", method = RequestMethod.GET)
    public ResponseEntity<?> getCurrentAsignatures(){
        try {
            return new ResponseEntity<>(materiaServices.getCurrentMaterias(), HttpStatus.ACCEPTED);
        } catch (Exception e) {
            return new ResponseEntity<>(e.getLocalizedMessage(), HttpStatus.NOT_FOUND);
        }
    }
    
    @RequestMapping(path = "/classifyStudent/{subj}", method = RequestMethod.GET)
    public ResponseEntity<?> getClassificationCancelSubject( @PathVariable String subj, 
            @RequestParam("grade1") double note1, @RequestParam("grade2") double note2 ){
        try {
            return new ResponseEntity<>(materiaServices.classifyStudent(subj, note1, note2), 
                    HttpStatus.ACCEPTED);
        } catch (Exception e) {
            e.printStackTrace();
            return new ResponseEntity<>(e.getLocalizedMessage(), HttpStatus.NOT_FOUND);
        }
    }
    
    @RequestMapping(path = "/estimate", method = RequestMethod.POST)
    public ResponseEntity<?> makeEstimateAsignature(@RequestBody Materia materia){
        try {
            return new ResponseEntity<>(materiaServices.getEstimate(materia), HttpStatus.CREATED);
        } catch (Exception e) {
            return new ResponseEntity<>(e.getLocalizedMessage(), HttpStatus.FORBIDDEN);
        }
    }
}
