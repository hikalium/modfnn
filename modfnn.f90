module modfnn
  use iso_fortran_env
  implicit none
  type matptr
    real(8), dimension(:, :), pointer :: p
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
    ! 新たにallocして返す。
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
  end function
end module

