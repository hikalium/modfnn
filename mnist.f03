program main
  use iso_fortran_env
  use modfnn
  implicit none
  !
  integer :: mnist_data_count = 1000
  !
  type(matptr), allocatable :: labels(:)
  type(matptr), allocatable :: imgs(:)
  integer :: i

  call read_MNIST(mnist_data_count, imgs, labels)

  do i = 1, mnist_data_count
    call disp(labels(i)%p)
    call disp_MNISTimg(imgs(i)%p)
  end do

contains
end program
