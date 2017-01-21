program main
  use modfnn
  implicit none

  real(8) :: X(1, 2) = reshape((/ 1.0, 0.5 /), (/1, 2/))
  real(8) :: W1(2, 3) = reshape( &
    (/ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6 /), (/2, 3/))
  real(8) :: B1(1, 3) = reshape((/ 0.1, 0.2, 0.3 /), (/1, 3/))

  call disp(X) 
  call disp(W1) 
  call disp(B1) 

  call disp(sigmoid(matmul(X, W1) + B1, size(B1, 2), size(B1, 1)))

end program
