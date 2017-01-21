module modfnn
  implicit none

contains
  function sigmoid(x) result(r)
    double precision :: x, r
    r = 1 / (1 + exp(-x))
  end function
  function relu(x) result(r)
    double precision :: x, r
    r = max(x, 0d0)
  end function

end module
