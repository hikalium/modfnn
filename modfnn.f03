module modfnn
  use iso_fortran_env
  implicit none
  type matptr
    real(8), dimension(:, :), pointer :: p
  end type
  type network
    type(matptr), allocatable :: W(:)     ! 重み
    type(matptr), allocatable :: b(:)     ! バイアス
    integer, allocatable :: layerSize(:)  ! 各レイヤのサイズ
  end type
  interface disp
    module procedure disp_m
    module procedure disp_r
    module procedure disp_u8
    module procedure disp_u32
  end interface 
contains
  function relu(x) result(r)
    real(8) :: x, r
    r = max(x, 0d0)
  end function
  subroutine disp_m(mat)
    real(8) mat(:, :)
    integer i
    do i = 1, size(mat, 1)
      write(*, "(100e12.4)") mat(i, 1:)
    end do
  end subroutine
  subroutine disp_r(r)
    real(8) r
    write(*, *) r
  end subroutine
  subroutine disp_u8(r)
    integer (int8) r
    write(*, *) r
  end subroutine
  subroutine disp_u32(r)
    integer (int32) r
    write(*, *) r
  end subroutine
  subroutine allocRandomMatrix(m, ys, xs)
    real(8), intent(inout), allocatable :: m(:,:)
    integer, intent(in) :: ys, xs
    integer :: c, seedsize
    integer, allocatable :: seed(:)
    if (allocated(m)) then
      deallocate(m)
    end if
    allocate(m(ys,xs))
    call system_clock(count=c)
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    seed = c
    call random_seed(put=seed)
    call random_number(m)
  end subroutine
  subroutine setRandomMatrixAllocated(p, ys, xs)
    real(8), intent(out), pointer :: p(:, :)
    real(8), allocatable, target :: m(:,:)
    integer, intent(in) :: ys, xs
    call allocRandomMatrix(m, ys, xs)
    p => m
  end subroutine
  subroutine tic(t1)
    integer, intent(inout) :: t1
    call system_clock(t1)
  end subroutine tic
  subroutine toc(t1)
    integer, intent(inout) :: t1
    integer :: t2, t_rate, t_max, diff
    call system_clock(t2,t_rate,t_max)
    if (t2 < t1) then
    diff = (t_max - t1) + t2 + 1
    else
    diff = t2 - t1
    end if
    write(*,*) "Time = ", diff/dble(t_rate)
    t1 = diff
  end subroutine toc
  function sigmoid(m) result(r)
    real(8), intent(in) :: m
    real(8) :: r
    r = 1d0 / (1d0 + exp(-m))
  end function
  function sigmoid_m(m) result(r)
    ! 新たにallocして返す。
    real(8), intent(in) :: m(:, :)
    !
    integer :: y, x
    integer :: xs, ys
    real(8), allocatable :: r(:, :)
    !
    xs = size(m, 2)
    ys = size(m, 1)
    allocate(r(ys, xs))
    do y = 1, ys
      do x = 1, xs
        r(y, x) = sigmoid(m(y, x))
      end do 
    end do    
  end function
  function sigmoid_v(v) result(r)
    ! 新たにallocして返す。
    real(8), intent(in) :: v(:)
    !
    integer :: i
    integer :: s
    real(8), allocatable :: r(:)
    !
    s = size(v)
    allocate(r(s))
    do i = 1, s
        r(i) = sigmoid(v(i))
    end do    
  end function
  function softmax_m(m) result(r)
    ! 新たにallocして返す。
    real(8), intent(in) :: m(:, :)
    !
    integer :: y, x
    integer :: xs, ys
    real(8), allocatable :: r(:, :)
    real(8) :: vs
    !
    xs = size(m, 2)
    ys = size(m, 1)
    allocate(r(ys, xs))
    r = m
    ! オーバーフロー対策
    vs = maxval(r)
    do y = 1, ys
      do x = 1, xs
        r(y, x) = r(y, x) - vs
      end do 
    end do
    ! 本来の計算
    do y = 1, ys
      do x = 1, xs
        r(y, x) = exp(r(y, x))
      end do 
    end do
    vs = sum(r)
    do y = 1, ys
      do x = 1, xs
        r(y, x) = r(y, x) / vs
      end do 
    end do    
  end function
  function mean_squared_error(v, t) result(rv)
    real(8), intent(in) :: v(:)
    real(8), intent(in) :: t(:)
    !
    integer :: i, cnt
    real(8), allocatable :: r(:)
    real(8) :: rv
    !
    cnt = size(v, 1)
    allocate(r(cnt))
    r = v - t
    r = r * r
    rv = sum(r) * 0.5d0
    deallocate(r)
  end function
  function cross_entropy_error(m, t) result(rv)
    real(8), intent(in) :: m(:, :)
    real(8), intent(in) :: t(:, :)
    !
    real(8) :: rv, delta = 1d-7
    !
    rv = -sum(t * log(m + delta)) / size(m, 1)
  end function
  subroutine numerical_gradient(f, m0, grad)
    interface
      real(8) function f(m)
        real(8), intent(in) :: m(:, :)
      end function
    end interface
    real(8) :: h = 1d-4
    real(8), allocatable, intent(out) :: grad(:, :)
    real(8), intent(in) :: m0(:, :)
    real(8), allocatable:: m(:, :)
    real(8) :: org_val, fxh1, fxh2
    integer :: size_x, size_y, x, y
    allocate(grad(size(m0, 1), size(m0, 2)))
    allocate(m(size(m0, 1), size(m0, 2)))
    m = m0
    size_y = size(m, 1)
    size_x = size(m, 2)
    do y = 1, size_y
      do x = 1, size_x
        org_val = m(y, x)
        ! calc f(x + h)
        m(y, x) = org_val + h
        fxh1 = f(m)
        ! calc f(x - h)
        m(y, x) = org_val - h
        fxh2 = f(m)
        ! calc grad
        grad(y, x) = (fxh1 - fxh2) / (2 * h)
      end do
    end do
  end subroutine
  subroutine initWithTwoLayer(net, isize, hsize, osize)
    type(network), allocatable, intent(out) :: net
    integer, intent(in) :: isize, hsize, osize
    allocate(net)
    allocate(net%W(2))
    allocate(net%b(2))
    allocate(net%layerSize(3))

    net%layerSize(1) = isize
    net%layerSize(2) = hsize
    net%layerSize(3) = osize
 
    call setRandomMatrixAllocated(net%W(1)%p, isize, hsize)
    call setRandomMatrixAllocated(net%b(1)%p, 1, hsize)
    call setRandomMatrixAllocated(net%W(2)%p, hsize, osize)
    call setRandomMatrixAllocated(net%b(2)%p, 1, osize)
  end subroutine
  function predictWithTwoLayer(net, x) result(y)
    type(network), intent(in) :: net
    real(8) :: x(:, :)
    real(8) :: a1(1, net%layerSize(2))
    real(8) :: a2(1, net%layerSize(3))
    real(8) :: y(1, net%layerSize(3)) 
    a1 = matmul(x, net%W(1)%p) + net%b(1)%p
    a1 = sigmoid_m(a1)
    a2 = matmul(a1, net%W(2)%p) + net%b(2)%p
    y = softmax_m(a2)
  end function
  function lossWithTwoLayer(net, x, t) result(v)
    ! t is train data
    type(network), intent(in) :: net
    real(8) :: v
    real(8) :: x(:, :)
    real(8) :: t(:, :)
    real(8) :: y(1, net%layerSize(3))
    y = predictWithTwoLayer(net, x)
    v = cross_entropy_error(y, t)
  end function
end module

