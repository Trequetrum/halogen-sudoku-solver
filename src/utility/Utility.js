"use strict";

//***********
//* fst: Returns the first component of a tuple.
//* snd: Returns the second component of a tuple.
//* actions: Array (Tuple number (a -> a)) that describes how
//*   to mutate an array at each given index
//* actOn: Array that should be copied and mutated.
//***********
export const modifyPerIndex_impl = 
       function(fst){
return function(snd){
return function(actions){
return function(actOn){

  var copy = actOn.slice();
  actions.forEach(function(action){
    var index = fst(action);
    if(copy[index] != null){
      copy[index] = snd(action)(copy[index]);
    }
  });

  return copy;
}}}}

//***********
//* fst: Returns the first component of a tuple.
//* snd: Returns the second component of a tuple.
//* actions: Array (Tuple number::Index number::Int) that describes how
//*   to bitMask an array at each given index
//* actOn: Array that should be copied and mutated.
//***********
export const dropMaskPerIndex_impl = 
       function(fst){
return function(snd){
return function(actions){
return function(actOn){

  var copy = actOn.slice();
  actions.forEach(function(action){
    var index = fst(action);
    if(copy[index] != null){
      copy[index] = ((~snd(action)) & copy[index]) | 0;
    }
  });

  return copy;
}}}}