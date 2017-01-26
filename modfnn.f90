module modfnn
  implicit none
contains
  function relu(x) result(r)
    real(8) :: x, r
    r = max(x, 0d0)
  end function
  subroutine disp(mat)
    real(8) mat(:, :)
    integer i
    do i = 1, size(mat, 1)
      write(*, "(100e12.4)") mat(i, 1:)
    end do
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
    real(8), intent(in) :: m(:, :)
    !
    integer :: y, x
    integer :: xs, ys
    real(8), allocatable :: r(:, :)
    !
    xs = size(m, 2)
    ys = size(m, 1)
    allocate(r(ys, xs))
    do y = 1, size(m, 1)
      do x = 1, size(m, 2)
        r(y, x) = sigmoid(m(y, x))
      end do 
    end do    
  end function
end module

