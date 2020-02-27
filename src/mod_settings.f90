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
    
    integer nL ! number of neurons
    integer, pointer :: ll(:)  ! neuron lengths
    integer, pointer :: aft(:) ! activation function type
    integer exportVerifyData ! yes/no
    integer exportAsciiNNData ! yes/no

    ! random number size
    real(rk) rndScale

    ! gradient descent data
    real(rk) lr ! learning rate
    integer nIter ! number of training iterations

    ! genetic optimization
    integer gaNchrom
    real(rk) gaCrossOverRate
    real(rk) gaMutationRate

    character(100) fnTDcsv,fnVDcsv,fnNNout,fnInp

  end type

  type (SettingsType) :: stg     ! settings

contains

! ---------------------------------------------------------------------------------
  subroutine readSettings()
  
    use String_Utility
    character KeyWord*64,OneLine*255,dummy*64,cdata*64
    character(50), allocatable :: saft(:)
    integer lun,i

    stg%fnInp = "nn.inp"
    lun = 12
    open (lun,FILE=trim(stg%fnInp),err=10,status='OLD')

!
!   Set up default settings:
!
    stg%fnTDcsv="nn-td.csv"
    stg%fnVDcsv="nn-vd.csv"
    stg%fnNNout="nn-out.txt"
    stg%testCase = tdSin
    stg%ntd=66
    stg%nvd=100
    stg%optAlgo = nnOptGenetic
    stg%nIter = 10000
    stg%rndScale = 30.0_rk

    stg%lr = 0.01_rk  
    stg%gaNchrom = 100
    stg%gaCrossOverRate = 0.9_rk  
    stg%gaMutationRate = 0.2_rk  
    
    stg%exportVerifyData = nnYes
    stg%exportAsciiNNData = nnYes   
    stg%trainDataXdist = tdXlin
    stg%verifyDataXdist = tdXlin

!
!   Read
!
    call rOneTL(lun,OneLine)
    do while (OneLine(1:3).NE.'EOF')
      read (OneLine,*) KeyWord
!
!     Reading keywords
!
      if (StrLowCase(TRIM(KeyWord)).EQ.'testcase') THEN
        READ(OneLine,*) dummy,cdata
        select case (StrLowCase(TRIM(cdata)))
        case ("sinus") 
          stg%testCase = tdSin
        case ("cosinus") 
          stg%testCase = tdCos
        case ("linear") 
          stg%testCase = tdLin
        case ("parabola") 
          stg%testCase = tdPar
        case ("sinusnoise") 
          stg%testCase = tdSinNoise                                                          
        case default
          print *,"input file ERROR in TestCase"
        end select

      else if (StrLowCase(TRIM(KeyWord)).EQ.'traindataxdist') THEN
        READ(OneLine,*) dummy,cdata
        select case (StrLowCase(TRIM(cdata)))
        case ("linear") 
          stg%trainDataXdist = tdXlin
        case ("random") 
          stg%trainDataXdist = tdXrnd
        case default
          print *,"input file ERROR in trainDataXdist"
        end select        
        
      else if (StrLowCase(TRIM(KeyWord)).EQ.'verifydataxdist') THEN
        READ(OneLine,*) dummy,cdata
        select case (StrLowCase(TRIM(cdata)))
        case ("linear") 
          stg%verifyDataXdist = tdXlin
        case ("random") 
          stg%verifyDataXdist = tdXrnd
        case default
          print *,"input file ERROR in verifyDataXdist"
        end select        
                

      else if (StrLowCase(TRIM(KeyWord)).EQ.'optimizationalgorithm') THEN
        READ(OneLine,*) dummy,cdata
        select case (StrLowCase(TRIM(cdata)))
        case ("montecarlo") 
          stg%optAlgo = nnOptMonteCarlo
        case ("genetic") 
          stg%optAlgo = nnOptGenetic
        case ("gradientdescent") 
          stg%optAlgo = nnOptGradDesc
        case default
          print *,"input file ERROR in OptimizationAlgorithm"
        end select
      else if (StrLowCase(TRIM(KeyWord)).EQ.'neuralnetsetup') THEN
        READ(OneLine,*) dummy,stg%nL
        allocate (stg%ll(stg%nL),stg%aft(stg%nL-1),saft(stg%nL-1))
        READ(OneLine,*) dummy,stg%nL,(stg%ll(i),i=1,stg%nL),(saft(i),i=1,stg%nL-1)
        do i  = 1, stg%nL-1
          select case (StrLowCase(TRIM(saft(i))))
          case ("sigmoid")
            stg%aft(i)=nnSigmoid
          case ("identity")
            stg%aft(i)=nnIdentity
          case ("relu")
            stg%aft(i)=nnRELU            
          case default
            print *,"input file ERROR in NeuralNetSetup"
          end select
        end do

      else if (StrLowCase(TRIM(KeyWord)).EQ.'ntraindata') THEN
        READ(OneLine,*) dummy,stg%ntd
      else if (StrLowCase(TRIM(KeyWord)).EQ.'nverifydata') THEN
        READ(OneLine,*) dummy,stg%nvd        
      else if (StrLowCase(TRIM(KeyWord)).EQ.'niterations') THEN
        READ(OneLine,*) dummy,stg%nIter             
      else if (StrLowCase(TRIM(KeyWord)).EQ.'randomnumrange') THEN
        READ(OneLine,*) dummy,stg%rndScale    
        
      else if (StrLowCase(TRIM(KeyWord)).EQ.'learningrate') THEN
        READ(OneLine,*) dummy,stg%lr
      else if (StrLowCase(TRIM(KeyWord)).EQ.'populationsize') THEN
        READ(OneLine,*) dummy,stg%gaNchrom    
      else if (StrLowCase(TRIM(KeyWord)).EQ.'crossoverrate') THEN
        READ(OneLine,*) dummy,stg%gaCrossOverRate    
      else if (StrLowCase(TRIM(KeyWord)).EQ.'mutationrate') THEN
        READ(OneLine,*) dummy,stg%gaMutationRate    
      

      else
        print *,"WARNING :: ReadInputFile :: Unknown keyword : "//TRIM(KeyWord)
      end if
      call rOneTL(lun,OneLine)
    end do
    close(lun)

    return

10  print *,trim(stg%fnInp)//" not found!"
    stop

  end subroutine


  subroutine getSettings()


    ! file names
    stg%fnInp = "nn.inp"
    stg%fnTDcsv="nn-td.csv"
    stg%fnVDcsv="nn-vd.csv"
    stg%fnNNout="nn-out.txt"
    

    ! available test cases = tdSin, tdCos, tdLin, tdPar, tdSin2, tdSinNoise, tdParSin
    stg%testCase = tdSin

    ! number of train data points  
    stg%ntd=66

    ! number of verify data points  
    stg%nvd=100

    ! distribution of x points for training data = tdXlin, tdXrnd
    stg%trainDataXdist = tdXlin

    ! distribution of x points for verify data = tdXlin, tdXrnd
    stg%verifyDataXdist = tdXlin

    ! optimization algorithm = nnOptMonteCarlo, nnOptGradDesc
    !stg%optAlgo = nnOptMonteCarlo
    !stg%optAlgo = nnOptGradDesc
    stg%optAlgo = nnOptGenetic

    ! number of training iterations
    stg%nIter = 1000000

    ! random number size = scale * (r01 - 0.5)
    stg%rndScale = 30.0_rk

    ! learning rate
    stg%lr = 0.01_rk  

    !  genetic
    stg%gaNchrom = 100
    stg%gaCrossOverRate = 0.9_rk  
    stg%gaMutationRate = 0.2_rk  


   ! create neural network
!    stg%nL = 5
!    allocate (stg%ll(stg%nL),stg%aft(stg%nL-1))
!    stg%ll(1)=1
!    stg%ll(2)=5
!    stg%ll(3)=5
!    stg%ll(4)=5
!    stg%ll(5)=1
!    stg%aft(1)=nnSigmoid
!    stg%aft(2)=nnSigmoid 
!    stg%aft(3)=nnSigmoid 
!    stg%aft(4)=nnIdentity



!    stg%nL = 4
!    allocate (stg%ll(stg%nL),stg%aft(stg%nL-1))
!    stg%ll(1)=1
!    stg%ll(2)=10
!    stg%ll(3)=10
!    stg%ll(4)=1
!    stg%aft(1)=nnSigmoid
!    stg%aft(2)=nnSigmoid 
!    stg%aft(3)=nnIdentity
   
    ! define number of layers in the network ( = number of hidden layers + 2 )
    stg%nL = 3
    allocate (stg%ll(stg%nL),stg%aft(stg%nL-1))
    ! define length of neurons in each layer ( first layer = input, last layer = output)
    stg%ll(1)=1
    stg%ll(2)=5
    stg%ll(3)=1
    ! define activation function for layers 2, ..., output : nnSigmoid nnSoftPlus nnIdentity nnRELU
    stg%aft(1)=nnSigmoid
    stg%aft(2)=nnIdentity 



    ! export verify data to disk
    stg%exportVerifyData = nnYes
    stg%exportAsciiNNData = nnYes

  end subroutine

end module
