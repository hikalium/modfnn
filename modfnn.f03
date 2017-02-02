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
    module procedure disp_iv
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
  subroutine disp_iv(v)
    integer v(:)
    write(*, *) v
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
  function random_integer(vmin, vmax) result(r)
    integer, intent(in) :: vmin, vmax
    integer :: r
    real(8) tmp
    call random_number(tmp)
    r = floor(tmp * (vmax - vmin + 1)) + vmin
  end function
  function random_integer_array(vmin, vmax, len) result(r)
    integer, intent(in) :: vmin, vmax
    integer :: len, r(len)
    integer :: i
    do i = 1, len
      r(i) = random_integer(vmin, vmax)
    end do
  end function
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
  subroutine num_grad_of_network(net, f, px, m, t, grad)
    ! net: network data
    ! f: loss function
    ! px: input matrix
    ! m: network matrix(Weight or bias)
    ! t: train matrix
    ! grad: grad matrix (same shape of m, return value)
    type(network), intent(in) :: net
    interface
      real(8) function f(net, x, t) result(rv)
        import network
        type(network), intent(in) :: net
        real(8), intent(in) :: x(:, :)
        real(8), intent(in) :: t(:, :)
      end function
    end interface
    real(8) :: h = 1d-4
    real(8), allocatable, intent(out) :: grad(:, :)
    real(8), intent(inout) :: px(:, :)
    real(8), intent(inout) :: m(:, :)
    real(8), intent(in):: t(:, :)
    real(8) :: org_val, fxh1, fxh2
    integer :: size_x, size_y, x, y
    allocate(grad(size(m, 1), size(m, 2)))

    size_y = size(m, 1)
    size_x = size(m, 2)
    do y = 1, size_y
      do x = 1, size_x
        org_val = m(y, x)
        ! calc f(x + h)
        m(y, x) = org_val + h
        fxh1 = f(net, px, t)
        ! calc f(x - h)
        m(y, x) = org_val - h
        fxh2 = f(net, px, t)
        ! calc grad
        grad(y, x) = (fxh1 - fxh2) / (2 * h)
        m(y, x) = org_val
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
    real(8) :: a1(size(x, 1), net%layerSize(2))
    real(8) :: a2(size(x, 1), net%layerSize(3))
    real(8) :: y(size(x, 1), net%layerSize(3))
    a1 = matmul(x, net%W(1)%p) + net%b(1)%p
    a1 = sigmoid_m(a1)
    a2 = matmul(a1, net%W(2)%p) + net%b(2)%p
    y = softmax_m(a2)
  end function
  function lossWithTwoLayer(net, x, t) result(v)
    ! t is train data
    type(network), intent(in) :: net
    real(8), intent(in) :: x(:, :)
    real(8), intent(in) :: t(:, :)
    real(8) :: v
    real(8) :: y(1, net%layerSize(3))
    y = predictWithTwoLayer(net, x)
    v = cross_entropy_error(y, t)
  end function
  subroutine getBatchFromIndexList(base, batch, indexList)
    type(matptr), intent(in) :: base(:)
    real(8), allocatable, intent(out):: batch(:, :)
    integer, intent(in) :: indexList(:)
    integer :: i, bsize
    bsize = size(indexList)
    allocate(batch(bsize, size(base(1)%p, 2)))
    do i = 1, bsize
      batch(i, :) = base(indexList(i))%p(1, :)
    end do
  end subroutine
  function accuracyForBatch(net, binput, btrain) result(r)
    type(network), intent(in) :: net
    real(8), intent(in) :: binput(:, :)
    real(8), allocatable :: predicted(:, :)
    real(8), intent(in) :: btrain(:, :)
    integer :: i
    real(8) :: r
    allocate(predicted(size(btrain, 1), size(btrain, 2)))
    r = 0
    predicted = predictWithTwoLayer(net, binput)
    do i = 1, size(binput, 1)
      if(maxloc(predicted(i, :), 1) .eq. maxloc(btrain(i, :), 1)) then
        r = r + 1
      end if
    end do
    r = r / size(binput, 1)
  end function
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
    ! To use this function,
    ! get data from http://yann.lecun.com/exdb/mnist/
    ! and place files:
    !   'train-images-idx3-ubyte'
    !   'train-labels-idx1-ubyte'
    ! into:
    !   './data/'
    integer, intent(in) :: n
    integer :: fp_img, fp_label
    integer :: x, y, i
    type(matptr), allocatable :: imgs(:)
    type(matptr), allocatable :: labels(:)
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
      i = read_uint8(fp_img)
      !call disp(i)
    end do
    do x = 1, 8
      i = read_uint8(fp_label)
      !call disp(i)
    end do
    ! Read
    do i = 1, n
      allocate(imgs(i)%p(1, 28 * 28))
      do x = 1, 28*28
          imgs(i)%p(1, x) = read_uint8(fp_img) / 255d0
      end do
      allocate(labels(i)%p(1, 10))
      labels(i)%p = 0
      labels(i)%p(1, read_uint8(fp_label) + 1) = 1
      !call disp(labels(i)%p)
      !call disp_MNISTimg(imgs(i)%p)
    end do
    print '("read ",I8," datas from MNIST")', n
    close(fp_label)
    close(fp_img)
  end subroutine
  subroutine disp_MNISTimg(m0)
    real(8) :: m0(:, :)
    real(8) :: m(28, 28)
    integer x, y
    m = reshape(m0, (/28, 28/))
    do x = 1, size(m, 2)
      do y = 1, size(m, 1)
        if(m(y, x) .eq. 0) then
          write(*, fmt='(A2)', advance='no') " "
        else if(m(y, x) < 0.25d0) then
          write(*, fmt='(A2)', advance='no') "."
        else if(m(y, x) < 0.5d0) then
          write(*, fmt='(A2)', advance='no') ","
        else if(m(y, x) < 0.75d0) then
          write(*, fmt='(A2)', advance='no') "x"
        else 
          write(*, fmt='(A2)', advance='no') "#"
        end if
      end do
      write(*, fmt='()')
    end do
      write(*, fmt='()')
  end subroutine
end module

