program main
  use iso_fortran_env
  use modfnn
  implicit none

  real(8) :: X(1, 2) = reshape((/ 1.0, 0.5 /), (/1, 2/))
  real(8) :: Y(1, 3) = reshape((/ 0.3d0, 2.9d0, 4.0d0 /), (/1, 3/))
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
  call disp(softmax_m(Y))
  call disp(sum(softmax_m(Y)))

  call read_MNIST()

contains
  function read_uint8(fp) result(v)
    integer (int8) :: v8
    integer v;
    integer, intent(in) :: fp;
    read(fp) v8
    v = v8
    if(v < 0) then
      v = 256 + v
    end if
  end function
  subroutine read_MNIST()
    ! Get data from http://yann.lecun.com/exdb/mnist/
    ! and place files:
    !   'train-images-idx3-ubyte'
    !   'train-labels-idx1-ubyte'
    ! into:
    !   './data/'
    integer :: fp, x, y, i
    type(matptr) :: imgs(3)
    fp = 10
    open(fp, file="data/train-images-idx3-ubyte", &
      form="unformatted", access="stream")
    ! Skip headers
    do x = 1, 16
      call disp(read_uint8(fp))
    end do
    ! Read
    allocate(imgs(1)%p(28, 28))
    do i = 1, 10
      do y = 1, 28
        do x = 1, 28
          imgs(1)%p(y, x) = read_uint8(fp)
        end do
      end do
      call disp_MNISTimg(imgs(1)%p)
    end do
    close(fp)
  end subroutine
  subroutine disp_MNISTimg(m)
    real(8) :: m(:, :)
    integer x, y
    do y = 1, size(m, 1)
      do x = 1, size(m, 2)
        if(m(y, x) .eq. 0) then
          write(*, fmt='(A2)', advance='no') " "
        else if(m(y, x) < 16) then
          write(*, fmt='(A2)', advance='no') "."
        else 
          write(*, fmt='(A2)', advance='no') "x"
        end if
      end do
      write(*, fmt='()')
    end do
      write(*, fmt='()')
  end subroutine
end program
