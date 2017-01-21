program main
  use modfnn
  implicit none

  print *, relu(0d0)  
  print *, relu(4d0)  
  print *, relu(2d0)  
  print *, relu(-4d0)  
  print *, relu(-2d0)  

end program
