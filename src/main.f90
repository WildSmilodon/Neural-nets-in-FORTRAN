program nnTest
  use mCommon      ! provides parameters and object type definitions
  use mSettings    ! provides the stg object, which hold all neural net and test case settings
  use mTrainingData
  use mNeuralNet
  implicit none
   
  
  type (NeuralNet) :: nn         ! neural net
  type (TrainingDataType) :: td  ! training data
  type (TrainingDataType) :: vd  ! verify data

  ! init random generator
  call seed_rnd_generator()

  ! get settings 
  call getSettings()

  ! create neural net object
  nn = nnCreate()

  ! show optimization problem size
  print *,"Number of weights and biases = ",nn%nProblemSize

  ! create training data
  td = tdCreate(stg%ntd,nn%inpLength,nn%outLength,stg%testCase,stg%trainDataXdist)

  ! create verify data
  vd = tdCreate(stg%nvd,nn%inpLength,nn%outLength,stg%testCase,stg%verifyDataXdist)

  ! train the nerural net
  call nnTrain(nn,td)

  ! verify results
  print *,"L2 norm of training data = ",nn%L2norm 
  print *,"L2 norm of verify data = ",nnCost(nn,vd)

  ! export verify data to disk
  if (stg%exportVerifyData.EQ.nnYes) then 
    call nnExport(nn,td,"nn-td.csv") 
    call nnExport(nn,vd,"nn-vd.csv") 
  end if
    
end program nnTest
