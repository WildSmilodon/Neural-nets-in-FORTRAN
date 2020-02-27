module mCommon 
  implicit none

!
! Real number representation (4=single, 8=double)
!    
  integer,  parameter :: rk = selected_real_kind(8)
  real(rk), parameter :: pi = 3.141592653589793238462643383_rk
!
! Parameters used to set values
!    
  integer, parameter :: nnYes = 1
  integer, parameter :: nnNo = 0

  integer, parameter :: nnSigmoid = 101
  integer, parameter :: nnRELU = 102
  integer, parameter :: nnIdentity = 103
  integer, parameter :: nnSoftPlus = 104

  integer, parameter :: nnOptGradDesc = 151
  integer, parameter :: nnOptMonteCarlo = 152
  integer, parameter :: nnOptGenetic = 153

  integer, parameter :: tdSin = 201
  integer, parameter :: tdLin = 202
  integer, parameter :: tdCos = 203
  integer, parameter :: tdPar = 204
  integer, parameter :: tdXlin = 205
  integer, parameter :: tdXrnd = 206
  integer, parameter :: tdSin2 = 207
  integer, parameter :: tdSinNoise = 208
  integer, parameter :: tdParSin = 209
      
      

! ----------------------------------------------------------------------
  type Matrix
!
!   Defines the matrix type
!    
! ----------------------------------------------------------------------  
    integer nrow,ncol 
    real(rk), pointer :: val(:,:)  ! id's of nodes
  end type
! ----------------------------------------------------------------------
  type Vector
!
!   Defines the vector type
!    
    integer nrow
    real(rk), pointer :: val(:)
  end type
! ----------------------------------------------------------------------
  type NeuralNet
!
!   Defines the neural net object
!    
    real(rk) lr    ! neural network learning rate
    integer nLayers ! total number of neruons (including input and output)
    integer nHiddenLayers  ! nLayers - 2
    integer nSteps         !  nLayers - 1, number of steps to be performed during eval phase ( = nLayers - 1)
    integer nProblemSize ! total number of weights and biases
    integer inpLength,outLength  ! number of rows in first and last layer
    integer, pointer :: AFtype(:)  ! type of activation function
    real(rk) L2norm ! final L2 norm after training
    type(Matrix), pointer :: weight(:)  ! weight matrices (nLayer-1)
    type(Vector), pointer :: neuron(:)  ! neurons (nLayer) 
    type(Vector), pointer :: error(:)
    type(Vector), pointer :: bias(:)
  end type


! ----------------------------------------------------------------------
  type TrainingDataType
!
!   Defines the training data type
!    
    integer n
    integer inpLength,outLength
    real(rk), pointer :: x(:,:)  ! (inpLength,n)
    real(rk), pointer :: y(:,:)  ! (outLength,n)
  end type
! ----------------------------------------------------------------------
  
contains

!
! -------------------------------------------------------------------------------------------------
!
real(rk) function getRandom(scale)
  real(rk) r,scale
  call random_number(r)
  getRandom = scale * (r - 0.5_rk)
end function

!
! -------------------------------------------------------------------------------------------------
!
real(rk) function getRandom01()
  real(rk) r
  call random_number(r)
  getRandom01 = r
end function

!
! -------------------------------------------------------------------------------------------------
!
integer function getRandom1N(N)
  integer N
  real(rk) r
  r = getRandom01()
  getRandom1N = floor ( r * N ) + 1
end function


! ----------------------------------------------------------------------
  subroutine ATxV(a,nrow,ncol,v,res)
!
!   res = transpose(a) * v
!    
    integer, intent(in) :: nrow,ncol
    real(rk), intent(in)  :: a(nrow,ncol)
    real(rk), intent(in)  :: v(nrow)
    real(rk), intent(out) :: res(ncol)
    integer r,c

    do r = 1, ncol
      res(r) = 0.0_rk
    end do

    do c = 1, nrow
      do r = 1, ncol        
        res(r) = res(r) + a(c,r)*v(c)
      end do
    end do

  end subroutine
! ----------------------------------------------------------------------

  subroutine seed_rnd_generator()
!
!   seed the random number generator with current time
!    
    integer, allocatable :: seed(:)
    integer size,i
    
    call random_seed(size=size)
    allocate(seed(size))
    do i=1,size
      call system_clock(seed(i))
    end do
    call random_seed(put=seed)

  end subroutine

end module
