module mSettings
  use mCommon
  implicit none

  type SettingsType

    integer testCase ! available test cases = tdSin, tdCos, tdLin, tdPar
    integer ntd ! number of train data points  
    integer nvd ! number of train verify points  
    integer trainDataXdist ! distribution of x points for training data = tdXlin, tdXrnd
    integer verifyDataXdist ! distribution of x points for verify data = tdXlin, tdXrnd
    integer optAlgo ! optimization algorithm = nnOptMonteCarlo, nnOptGradDesc
    integer nIter ! number of training iterations
    real(rk) lr ! learning rate
    integer nL ! number of neurons
    integer, pointer :: ll(:)  ! neuron lengths
    integer, pointer :: aft(:) ! activation function type
    integer exportVerifyData ! yes/no

  end type

contains

  subroutine getSettings(stg)

    type (SettingsType) :: stg

    ! available test cases = tdSin, tdCos, tdLin, tdPar, tdSin2, tdSinNoise, tdParSin
    stg%testCase = tdParSin

    ! number of train data points  
    stg%ntd=666

    ! number of verify data points  
    stg%nvd=100

    ! distribution of x points for training data = tdXlin, tdXrnd
    stg%trainDataXdist = tdXlin

    ! distribution of x points for verify data = tdXlin, tdXrnd
    stg%verifyDataXdist = tdXlin

    ! optimization algorithm = nnOptMonteCarlo, nnOptGradDesc
    !stg%optAlgo = nnOptMonteCarlo
    stg%optAlgo = nnOptGradDesc

    ! number of training iterations
    stg%nIter = 50000

    ! learning rate
    stg%lr = 0.001_rk  

    ! create neural network
    stg%nL = 4
    allocate (stg%ll(stg%nL),stg%aft(stg%nL-1))
    stg%ll(1)=1
    stg%ll(2)=10
    stg%ll(3)=10
    stg%ll(4)=1
    stg%aft(1)=nnSigmoid
    stg%aft(2)=nnSigmoid 
    stg%aft(3)=nnIdentity
    
!    ! define number of layers in the network ( = number of hidden layers + 2 )
!    stg%nL = 3
!    allocate (stg%ll(stg%nL),stg%aft(stg%nL-1))
!
!    ! define length of neurons in each layer ( first layer = input, last layer = output)
!    stg%ll(1)=1
!    stg%ll(2)=5
!    stg%ll(3)=1
!    ! define activation function for layers 2, ..., output : nnSigmoid nnSoftPlus nnIdentity nnRELU
!    stg%aft(1)=nnSigmoid
!    stg%aft(2)=nnIdentity 
!
!    ! export verify data to disk
    stg%exportVerifyData = nnYes

  end subroutine

end module