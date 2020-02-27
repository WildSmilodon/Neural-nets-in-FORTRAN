
! ----------------------------------------------------------------------  
subroutine ga(nn,td)
  use mGenetic
  use mNeuralNet
  use mCommon
  use mSettings
  implicit none

  type(PopulationType) :: pop
  type(PopulationType) :: newPop,oldPop

  type(NeuralNet) nn
  type(TrainingDataType) :: td
  integer best,gaEvaluatePopulation
  integer i,j,worstChild
  real(rk) minObjF,maxObjF
  logical notDone

  pop%Ngenes = nn%nProblemSize
  pop%Nchrom = stg%gaNchrom
  pop%CrossOverRate = stg%gaCrossOverRate
  pop%MutationRate = stg%gaMutationRate
  call gaGenerateInitialPopulation(pop)

  ! prepare a copy of the population
  newPop%Ngenes = pop%Ngenes
  newPop%Nchrom = pop%Nchrom
  newPop%CrossOverRate = pop%CrossOverRate
  newPop%MutationRate = pop%MutationRate
  call gaGenerateInitialPopulation(newPop);

  ! prepare a copy of the population
  oldPop%Ngenes = pop%Ngenes
  oldPop%Nchrom = pop%Nchrom
  oldPop%CrossOverRate = pop%CrossOverRate
  oldPop%MutationRate = pop%MutationRate
  call gaGenerateInitialPopulation(oldPop);


  ! evaluate population & find the best chromosome
  best = gaEvaluatePopulation(pop,nn,td)
  minObjF = pop%chromosomes(best)%objF
  call gaCopyChromosome(pop%chromosomes(best),pop%best)

  ! start genetic optimization loop
  do i = 1,stg%nIter
    ! evaluate fitness
    do j = 1,pop%Nchrom
      call gaEvaluateFitness(pop%chromosomes(j)) 
      call gaCopyChromosome(pop%chromosomes(j),oldPop%chromosomes(j)) ! pop -> oldPop
    end do
    ! evaluate total fitness
    call gaEvaluteTotalFintess(pop) 
    ! evaluate Cumulative Probability for chromosomes 
    call gaEvaluteCumulativeProbability(pop)

!    do j = 1,pop%Nchrom
!      print *,i,pop%chromosomes(j)%objF,pop%chromosomes(j)%fitness,pop%chromosomes(j)%cumProb
!    end do
!
    ! set up new population by choosing fittest cromosomes
    call gaChooseFittestChromosomes(pop,newPop)  ! new generation = newPop
    ! cross over
    call gaCrossOver(newPop,pop) ! new generation = pop
    ! mutate
    call gaMutate(pop)

    ! evaluate population 
    best = gaEvaluatePopulation(pop,nn,td)   

    ! overwrite children with better parents
    notDone = .true.

    do while (notDone)
      ! find the worst child
      maxObjF = -1.0_rk
      do j = 1,pop%Nchrom
        if ( pop%chromosomes(j)%objF > maxObjF ) then
          maxObjF = pop%chromosomes(j)%objF
          worstChild = j
        end if
      end do
      ! find a better parent
      notDone = .false.
      do j = 1,pop%Nchrom
        if (oldPop%chromosomes(j)%objF < pop%chromosomes(worstChild)%objF ) then
          notDone = .true.
          call gaCopyChromosome(oldPop%chromosomes(j),pop%chromosomes(worstChild))  ! parent -> child
          oldPop%chromosomes(j)%objF = 1.0E10_rk
          exit
        end if
      end do
    end do

    ! find the best chromosome
    best = gaEvaluatePopulation(pop,nn,td)   
    if (pop%chromosomes(best)%objF < minObjF ) then
      minObjF = pop%chromosomes(best)%objF
      call gaCopyChromosome(pop%chromosomes(best),pop%best)
      print *,i,pop%best%objF
    end if


    
    
  end do

  ! apply best chromosome ch -> nn
  call gaEvaluateObjF(pop%best,nn,td)

end subroutine ga
  
  
! ----------------------------------------------------------------------    
integer function gaEvaluatePopulation(p,nn,td)
  use mGenetic
  use mNeuralNet    
  use mCommon
  implicit none

  type(PopulationType) :: p
  type(NeuralNet) nn
  type(TrainingDataType) :: td
  real(rk) minObjF
  integer i,best
  minObjF = 1.0E10_rk
  best = -1
  do i = 1, p%Nchrom
    call gaEvaluateObjF(p%chromosomes(i),nn,td)
    if (p%chromosomes(i)%objF<minObjF) then
      minObjF = p%chromosomes(i)%objF
      best = i
    end if    
  end do
   
  gaEvaluatePopulation = best

  end function
  
  ! ----------------------------------------------------------------------    
subroutine gaEvaluateObjF(ch,nn,td)
  use mGenetic
  use mNeuralNet      
  use mCommon
  implicit none

      type(ChromosomeType) :: ch
      type(NeuralNet) nn
      type(TrainingDataType) :: td
     
      integer i,r,c,iGene
  
      iGene = 0
      do i = 1, nn%nSteps
        do r = 1,nn%bias(i)%nrow
          iGene = iGene + 1
          nn%bias(i)%val(r) =  ch%genes(iGene)%val
        end do
      end do
      do i = 1, nn%nSteps
        do c = 1,nn%weight(i)%ncol
          do r = 1,nn%weight(i)%nrow
            iGene = iGene + 1
            nn%weight(i)%val(r,c) =  ch%genes(iGene)%val
          end do
        end do
      end do
      
      ch%objF = nnCost(nn,td)
 
    end subroutine


! ----------------------------------------------------------------------  
subroutine gaRandomizeGene(g)
  use mGenetic
  use mSettings
  implicit none

  integer i

  type(GeneType) :: g

  g%val = getRandom(stg%rndScale)

!  i = getRandom1N(201) - 101
!  g%val = real(i)/10.0_rk
  
end subroutine gaRandomizeGene 
  
    