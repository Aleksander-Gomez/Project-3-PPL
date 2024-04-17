(defun subset-of (a b)
  "Check if list A is a subset of list B."
  (cond
    ((null a) t)                           
    ((null b) nil)                          
    ((member (car a) b)        
     (subset-of (cdr a) b))                
    (t nil)))                              
