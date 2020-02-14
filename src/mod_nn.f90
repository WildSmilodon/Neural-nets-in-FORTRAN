MODULE mNeuralNet
  use mCommon
  implicit none

!
! -------------------------------------------------------------------------------------------------
!
contains
!
! -------------------------------------------------------------------------------------------------
!
subroutine nnTrain(nn,td,optAlgo,nIter)

  type(NeuralNet) :: nn,nnC
  type(TrainingDataType) :: td
  integer, intent(in) :: optAlgo ! optimization algorithm type
  integer, intent(in) :: nIter ! number of iterations

  real(rk) co,minCo

  integer j

  select case (optAlgo)

    case (nnOptGradDesc) 
      !
      !  Gradient descent optimization strategy
      !
      do j=1,nIter
        call nnTrainGD(nn,td)
      end do

    case (nnOptMonteCarlo)
      !
      !  Monte Carlo optimization strategy
      !
      ! create copy of the neural net object
      nnC = nnCreateCopy(nn)
      ! copy data nn -> nnC
      call nnCopy(nn,nnC)
      
      minCo=111111.0_rk
      do j=1,nIter
        call nnRandomize(nnC)
        co=nnCost(nnC,td)
        if (co.lt.minCo) then ! better solution found
          minCo=co
          call nnCopy(nnC,nn) ! nnC -> nn
        end if
      end do

      
    case default
      print *, "ERROR" 

  end select

  ! evaluate L2 norm based on training data
  nn%L2norm = nnCost(nn,td) 

end subroutine
!
! -------------------------------------------------------------------------------------------------
!
real(rk) function nnActivationFunction(x,AFtype)

  real(rk) x,y
  integer AFtype

  select case (AFtype)

    case (nnSigmoid) 
      y = 1.0_rk / (1.0_rk + EXP(-x))

    case (nnRELU)
      if (x<0) then
        y = 0.0_rk
      else
        y = x
      end if

    case (nnIdentity) 
      y = x

    case (nnSoftPlus)
      y = log(1.0_rk + exp(x))

    case default
      print *, "ERROR" 

  end select
  nnActivationFunction = y

end function

!
! -------------------------------------------------------------------------------------------------
! calculate derivative of activation function
!
real(rk) function nnAFderivative(x,AFtype)

  real(rk) x,derAF
  integer AFtype

  select case (AFtype)

    case (nnSigmoid) 
      derAF = x * ( 1.0_rk - x ) 

    case (nnRELU)
      if (x<=0.0_rk) then
        derAF = 0.0_rk
      else
        derAF = 1.0_rk
      end if

    case (nnIdentity) 
      derAF = 1.0_rk

    case (nnSoftPlus)
      derAF =  1.0_rk / (1.0_rk + EXP(-x))

    case default
      print *, "ERROR" 

  end select

  nnAFderivative = derAF

end function


!
! -------------------------------------------------------------------------------------------------
!
subroutine nnExport(nn,vd,fname)

  character(*) fname
  type(NeuralNet), intent(in) :: nn
  type(TrainingDataType) :: vd
  real(rk), allocatable :: x(:),y(:)
  integer i,lun

  lun=12
  OPEN (lun,FILE=trim(fname),STATUS='UNKNOWN')
  WRITE (lun,'(A)') "X trueY nnY L2norm"

  allocate (x(nn%inpLength),y(nn%outLength))

  do i=1,vd%n
    call nnEval(nn,vd%x(:,i),y)
    write (lun,'(4G18.10)' ) vd%x(1,i),vd%y(1,i),y,nnL2norm(nn,vd%x(:,i),vd%y(:,i))
  end do  

  deallocate (x,y)
  close (lun)

end subroutine

!
! -------------------------------------------------------------------------------------------------
!

real(rk) function nnCost(nn,td)

  type(NeuralNet), intent(in) :: nn
  type(TrainingDataType) :: td
  integer i

  nnCost = 0.0_rk
  do i=1,td%n
    nnCost = nnCost + nnL2norm(nn,td%x(:,i),td%y(:,i))
  end do  
  nnCost = nnCost / real(td%n)

end function
!
! -------------------------------------------------------------------------------------------------
!
subroutine nnTrainGD(nn,td)

  type(NeuralNet), intent(in) :: nn
  type(TrainingDataType) :: td
  integer i,j,r,c
  real(rk) dw

  ! for each training data point
  do i=1,td%n

    ! evaluate neuron values in the neural net
    call nnSetNeurons(nn,td%x(:,i))

    ! calculate error and backpropagate it
    call nnBackPropError(nn,td%y(:,i))

    ! update weights by gradient descent
    do j = 1,nn%nSteps
      do r = 1,nn%weight(j)%nrow
        do c = 1,nn%weight(j)%ncol          
          dw = nn%lr * nn%error(j)%val(r) * nn%neuron(j)%val(c) * nnAFderivative(nn%neuron(j+1)%val(r),nn%AFtype(j)) 
          nn%weight(j)%val(r,c) = nn%weight(j)%val(r,c) + dw
        end do
      end do
    end do

    ! update bias  by gradient descent
    do j = 1,nn%nSteps
      do r = 1,nn%bias(j)%nrow
        dw = nn%lr * nn%error(j)%val(r) * nnAFderivative(nn%neuron(j+1)%val(r),nn%AFtype(j)) 
        nn%bias(j)%val(r) = nn%bias(j)%val(r) + dw
      end do
    end do

  end do  

end subroutine



!
! -------------------------------------------------------------------------------------------------
!
real(rk) function nnL2norm(nn,x,ytd)

  Type(NeuralNet), intent(in) :: nn
  real(rk), intent(in) :: x(nn%inpLength)
  real(rk), intent(in) :: ytd(nn%outLength) ! training data y
  real(rk), allocatable :: y(:)
  real(rk) :: result
  integer :: i

  allocate(y(nn%outLength))

  ! evaluate value using the neural net
  call nnEval(nn,x,y)

  ! calcualte norm
  result = 0.0_rk
  do i = 1, nn%outLength
    result = result + (y(i) - ytd(i))**2.0_rk
  end do

  nnL2norm = result

  deallocate (y)

end function nnL2norm

!
! -------------------------------------------------------------------------------------------------
!

subroutine nnBackPropError(nn,ytd)
!
!  Find error at output neuron and backpropagates it
!      
  Type(NeuralNet), intent(in) :: nn
  real(rk), intent(in) :: ytd(nn%outLength) ! training data y
  integer :: i

  ! calcualte error norm for output layer
  do i = 1, nn%outLength
    nn%error(nn%nSteps)%val(i) = ytd(i) - nn%neuron(nn%nLayers)%val(i) 
  end do

  ! backpropagate
  do i = nn%nSteps - 1, 1, -1
    ! multiply error by transposed matrix
     call ATxV(nn%weight(i+1)%val,nn%weight(i+1)%nrow,nn%weight(i+1)%ncol,nn%error(i+1)%val,nn%error(i)%val)
  end do

end subroutine


!
! -------------------------------------------------------------------------------------------------
!
subroutine nnSetNeurons(nn,x)

  Type(NeuralNet) nn
  real(rk) x(nn%inpLength)
  integer i,j
  ! copy x to first neuron
  do i = 1, nn%inpLength
    nn%neuron(1)%val(i) = x(i)
  end do
  do i = 1, nn%nSteps
    nn%neuron(i+1)%val = nn%bias(i)%val + MATMUL(nn%weight(i)%val,nn%neuron(i)%val)
    ! activate
    do j = 1, nn%neuron(i+1)%nrow
      nn%neuron(i+1)%val(j) = nnActivationFunction(nn%neuron(i+1)%val(j),nn%AFtype(i))
    end do
  end do

end subroutine


!
! -------------------------------------------------------------------------------------------------
!
subroutine nnEval(nn,x,y)

  Type(NeuralNet) nn
  real(rk) x(nn%inpLength)
  real(rk) y(nn%outLength)
  integer i

  ! evaluate neurons
  call nnSetNeurons(nn,x)
  ! copy last neuron to y
  do i = 1, nn%outLength
    y(i) = nn%neuron(nn%nLayers)%val(i)
  end do

end subroutine


!
! -------------------------------------------------------------------------------------------------
!
Type(NeuralNet) function nnCreateCopy(nn)

  type(NeuralNet) :: nn,nnC
  integer, allocatable :: ll(:)
  integer j

  allocate (ll(nn%nLayers))
  do j=1,nn%nLayers
    ll(j)=nn%neuron(j)%nrow
  end do
  nnC = nnCreate(nn%nLayers,ll,nn%AFtype,nn%lr)

  deallocate(ll)

  nnCreateCopy = nnC

end function


!
! -------------------------------------------------------------------------------------------------
!
subroutine nnCopy(nnFrom, nnTo)

  type(NeuralNet) :: nnFrom
  type(NeuralNet) :: nnTo

  integer i,r,c

  nnTo%lr = nnFrom%lr
  nnTo%nLayers = nnFrom%nLayers
  nnTo%nHiddenLayers = nnFrom%nHiddenLayers
  nnTo%nSteps = nnFrom%nSteps
  nnTo%nProblemSize = nnFrom%nProblemSize
  nnTo%inpLength = nnFrom%inpLength
  nnTo%outLength = nnFrom%outLength

  do i = 1, nnTo%nSteps
    nnTo%AFtype(i) = nnFrom%AFtype(i)   
    nnTo%error(i)%nrow = nnFrom%error(i)%nrow
    nnTo%bias(i)%nrow = nnFrom%bias(i)%nrow
    do r = 1, nnTo%error(i)%nrow
      nnTo%error(i)%val(r) = nnFrom%error(i)%val(r)
      nnTo%bias(i)%val(r) = nnFrom%bias(i)%val(r)
    end do
  end do

  do i = 1, nnTo%nLayers
    nnTo%neuron(i)%nrow = nnFrom%neuron(i)%nrow
    do r = 1, nnTo%neuron(i)%nrow
      nnTo%neuron(i)%val(r) = nnFrom%neuron(i)%val(r)
    end do
  end do

  do i = 1, nnTo%nSteps   
    nnTo%weight(i)%nrow = nnFrom%weight(i)%nrow
    nnTo%weight(i)%ncol = nnFrom%weight(i)%ncol
    do r = 1, nnTo%weight(i)%nrow
      do c = 1, nnTo%weight(i)%ncol
        nnTo%weight(i)%val(r,c) = nnFrom%weight(i)%val(r,c)
      end do
    end do
  end do

end subroutine nnCopy


!
! -------------------------------------------------------------------------------------------------
!
Type(NeuralNet) function nnCreate(nL,ll,aft,lr)

  INTEGER nL  ! number of layers
  real(rk) lr  ! learinign rate
  INTEGER aft(nL-1) ! activation function type
  integer ll(nL) ! layer lengths
  Type(NeuralNet) nn
  integer i
  
  nn%nLayers = nL
  nn%nSteps = nL - 1
  nn%nHiddenLayers = nL - 2
  nn%nProblemSize = 0
  nn%lr = lr
  ! activation function types
  allocate ( nn%AFtype(nn%nSteps) )
  do i = 1, nn%nSteps
    nn%AFtype(i) = aft(i)
  end do
  ! create neurons
  allocate ( nn%neuron(nn%nLayers) )
  do i = 1, nn%nLayers
    nn%neuron(i)%nrow = ll(i)
    allocate (nn%neuron(i)%val(nn%neuron(i)%nrow))
  end do
  
  ! create bias vectors
  allocate ( nn%bias(nn%nSteps) )
  do i = 1, nn%nSteps
    nn%bias(i)%nrow = ll(i+1)
    allocate (nn%bias(i)%val(nn%bias(i)%nrow))
    nn%nProblemSize = nn%nProblemSize + nn%bias(i)%nrow
  end do

! create error vectors
  allocate ( nn%error(nn%nSteps) )
  do i = 1, nn%nSteps
    nn%error(i)%nrow = ll(i+1)
    allocate (nn%error(i)%val(nn%error(i)%nrow))
  end do

  ! create weights
  allocate ( nn%weight(nn%nSteps) )
  do i = 1, nn%nSteps
    nn%weight(i)%nrow = ll(i+1)
    nn%weight(i)%ncol = ll(i)
    allocate ( nn%weight(i)%val(nn%weight(i)%nrow,nn%weight(i)%ncol) )
    nn%nProblemSize = nn%nProblemSize + nn%weight(i)%nrow*nn%weight(i)%ncol
  end do


  ! set weights and biases to random values
  call nnRandomize(nn)

  ! determine the length of input and output layer
  nn%inpLength = nn%neuron(1)%nrow
  nn%outLength = nn%neuron(nn%nLayers)%nrow

  nnCreate = nn

end function

!
! -------------------------------------------------------------------------------------------------
!
real(rk) function getRandom()
  real(rk) r
  call random_number(r)
  getRandom = 10.0_rk * r - 5.0_rk
  !getRandom = r - 0.5_rk
  !getRandom = 2.0_rk * (r - 0.5_rk)
end function


!
! -------------------------------------------------------------------------------------------------
!
subroutine nnRandomize(nn)
  Type(NeuralNet) nn
  integer i,r,c
  do i = 1, nn%nSteps
    do r = 1,nn%bias(i)%nrow
      nn%bias(i)%val(r) =  getRandom() 
    end do
  end do
  do i = 1, nn%nSteps
    do c = 1,nn%weight(i)%ncol
      do r = 1,nn%weight(i)%nrow
        nn%weight(i)%val(r,c) = getRandom()
      end do
    end do
  end do

end subroutine


end module
