program main
  use iso_fortran_env
  use modfnn
  implicit none
  !
  integer :: batch_size = 1
  integer :: batch_size_for_accuracy = 100
  integer :: mnist_data_count = 1000
  real(8) :: learning_rate = 0.1d0
  !
  type(network), allocatable :: net
  real(8), allocatable:: x(:, :)
  integer, allocatable :: batch_index(:)
  real(8), allocatable :: batch_label(:, :)
  real(8), allocatable :: batch_input(:, :)
  real(8), allocatable :: grad(:, :)
  type(matptr), allocatable :: labels(:)
  type(matptr), allocatable :: imgs(:)
  integer :: i

  allocate(batch_index(batch_size))

  call allocRandomMatrix(x, 2, 784)

  call read_MNIST(mnist_data_count, imgs, labels)
  call initWithTwoLayer(net, 784, 100, 10)
  !call disp(predictWithTwoLayer(net, x))

  do i = 1, 100
    ! 訓練用バッチを準備
    batch_index = random_integer_array(1, mnist_data_count, batch_size)
    call getBatchFromIndexList(labels, batch_label, batch_index)
    call getBatchFromIndexList(imgs, batch_input, batch_index)
    !call disp(batch_label)
    !call disp(maxloc(batch_label, 2))
    !call disp(net%W(2)%p)
    ! パラメータを勾配に基いて更新
    call num_grad_of_network(net, lossWithTwoLayer, batch_input, &
      net%W(2)%p, batch_label, grad)
    net%W(2)%p = net%W(2)%p - grad * learning_rate
    !
    call num_grad_of_network(net, lossWithTwoLayer, batch_input, &
      net%b(2)%p, batch_label, grad)
    net%b(2)%p = net%b(2)%p - grad * learning_rate
    ! 現時点の精度を表示
    batch_index = random_integer_array(1, mnist_data_count, batch_size_for_accuracy)
    call getBatchFromIndexList(labels, batch_label, batch_index)
    call getBatchFromIndexList(imgs, batch_input, batch_index)
    call disp(accuracyForBatch(net, batch_input, batch_label))
  
  end do
 
  !call getBatch(labels, )
contains
end program
