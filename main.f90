program main
  use iso_fortran_env
  use modfnn
  implicit none

  real(8) :: X(1, 2) = reshape((/ 1.0, 0.5 /), (/1, 2/))
  real(8) :: Y(1, 3) = reshape((/ 0.3d0, 2.9d0, 4.0d0 /), (/1, 3/))
  real(8) :: t(10, 1) = reshape((/ 0d0, 0d0, 1d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /), (/10, 1/))
  real(8) :: s(10, 1) = reshape((/ 0.1d0, 0.05d0, 0.6d0, 0.0d0, 0.05d0, 0.1d0, 0.0d0, 0.1d0, 0d0, 0d0 /), (/10, 1/))
  type(matptr) :: W(3)
  type(matptr) :: b(3)
  real(8), allocatable :: tmp(:, :)
  type(matptr), allocatable :: imgs(:)
  integer, allocatable :: labels(:)
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

  ! call read_MNIST(1000, imgs, labels)
  call disp(mean_squared_error(s(:, 1), t(:, 1)))

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
  subroutine read_MNIST(n, imgs, labels)
    ! Get data from http://yann.lecun.com/exdb/mnist/
    ! and place files:
    !   'train-images-idx3-ubyte'
    !   'train-labels-idx1-ubyte'
    ! into:
    !   './data/'
    integer, intent(in) :: n
    integer :: fp_img, fp_label
    integer :: x, y, i
    type(matptr), allocatable :: imgs(:)
    integer, allocatable :: labels(:)
    allocate(imgs(n))
    allocate(labels(n))
    fp_img = 10
    fp_label = 11
    open(fp_img, file="data/train-images-idx3-ubyte", &
      form="unformatted", access="stream")
    open(fp_label, file="data/train-labels-idx1-ubyte", &
      form="unformatted", access="stream")
    ! Skip headers
    do x = 1, 16
      call disp(read_uint8(fp_img))
    end do
    do x = 1, 8
      call disp(read_uint8(fp_label))
    end do
    ! Read
    do i = 1, n
      allocate(imgs(i)%p(28, 28))
      do y = 1, 28
        do x = 1, 28
          imgs(i)%p(y, x) = read_uint8(fp_img)
        end do
      end do
      labels(i) = read_uint8(fp_label)
      call disp(labels(i))
      call disp_MNISTimg(imgs(i)%p)
    end do
    close(fp_label)
    close(fp_img)
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
        else if(m(y, x) < 32) then
          write(*, fmt='(A2)', advance='no') ","
        else if(m(y, x) < 64) then
          write(*, fmt='(A2)', advance='no') "x"
        else 
          write(*, fmt='(A2)', advance='no') "#"
        end if
      end do
      write(*, fmt='()')
    end do
      write(*, fmt='()')
  end subroutine
end program
