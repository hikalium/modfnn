program main
  use modfnn
  implicit none

  real(8) :: X(1, 2) = reshape((/ 1.0, 0.5 /), (/1, 2/))
  type(matptr) :: W(3)
  type(matptr) :: b(3)
  real(8), allocatable :: tmp(:, :)
  !
  allocate(W(1)%p(2, 3))
  W(1)%p = reshape((/ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6 /), (/2, 3/))
  allocate(b(1)%p(3, 1))
  b(1)%p = reshape((/ 0.1, 0.2, 0.3 /), (/3, 1/))

  allocate(W(2)%p(3, 2))
  W(2)%p = reshape((/ 0.1, 0.2, 0.4, 0.5, 0.3, 0.6 /), (/3, 2/))
  allocate(b(2)%p(2, 1))
  b(2)%p = reshape((/ 0.1, 0.2 /), (/2, 1/))

  allocate(W(3)%p(2, 2))
  W(3)%p = reshape((/ 0.1, 0.2, 0.3, 0.4 /), (/2, 2/))
  allocate(b(3)%p(2, 1))
  b(3)%p = reshape((/ 0.1, 0.2 /), (/2, 1/))

  tmp = matmul(X, W(1)%p) + b(1)%p
  tmp = sigmoid_m(tmp)
  tmp = matmul(tmp, W(2)%p) + b(2)%p
  tmp = sigmoid_m(tmp)
  tmp = matmul(tmp, W(3)%p) + b(3)%p

  call disp(tmp)

end program
