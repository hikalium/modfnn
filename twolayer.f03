program main
  use iso_fortran_env
  use modfnn
  implicit none

  type(network), allocatable :: net
  real(8), allocatable:: x(:, :)

  call allocRandomMatrix(x, 1, 784)

  call initWithTwoLayer(net, 784, 100, 10)
  ! call read_MNIST(1000, imgs, labels)
  call disp(predictWithTwoLayer(net, x))

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
  function f2(m) result(v)
    real(8), intent(in) :: m(:, :)
    real(8) :: v
    v = sum(m * m)
  end function
end program
