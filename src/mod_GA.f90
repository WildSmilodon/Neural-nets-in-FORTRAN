MODULE mGenetic
  use mCommon
  use mNeuralNet
  implicit none

! ----------------------------------------------------------------------
  type GeneType
!
!   Defines the gene (which is a part of a chromosome)
!    
! ----------------------------------------------------------------------  
    real(rk) val
  end type

! ----------------------------------------------------------------------
  type ChromosomeType
!
!   Defines the chromosome, which is one individual in a population
!    
! ----------------------------------------------------------------------  
    type(GeneType), pointer :: genes(:)
    real(rk) objF
    real(rk) fitness
    real(rk) probability
    real(rk) cumProb ! cumulative probability for Roulette wheel

  end type

! ----------------------------------------------------------------------
  type PopulationType
!
!   Defines the chromosome, which is one individual in a population
!    
! ----------------------------------------------------------------------  
    type(ChromosomeType), pointer :: chromosomes(:)
    type(ChromosomeType) :: best
    real(rk) totalFitness;
    integer Ngenes;
    integer Nchrom;
    real(rk) CrossOverRate;
    real(rk) MutationRate;
    integer MutationNumber;

  end type


! ----------------------------------------------------------------------
contains

! ----------------------------------------------------------------------
  subroutine gaMutate(p) 
    type(PopulationType) :: p
    integer cr,ge,i

    do i=1,p%MutationNumber
      cr = getRandom1N(p%Nchrom)
      ge = getRandom1N(p%Ngenes)
      call gaRandomizeGene(p%chromosomes(cr)%genes(ge))
    end do

  end subroutine

! ----------------------------------------------------------------------  
  subroutine gaCrossOver(pop,newPop) 
    type(PopulationType) :: pop,newPop
    integer iAta,iMama,i,iCut
    real(rk) r
  
    do iAta = 1,pop%Nchrom
      r = getRandom01()
      if (r<pop%CrossOverRate) then
        ! this chromosome will do it!
        iMama = getRandom1N(pop%Nchrom)

!        iCut = getRandom1N(pop%Ngenes)
!        do i=1,pop%nGenes
!          if (i<iCut) then
!            newPop%Chromosomes(iAta)%genes(i)%val = pop%Chromosomes(iAta)%genes(i)%val
!          else
!            newPop%Chromosomes(iAta)%genes(i)%val = pop%Chromosomes(iMama)%genes(i)%val
!          end if
!        end do

        do i=1,pop%nGenes
          if (getRandom01()<0.5_rk) then
            newPop%Chromosomes(iAta)%genes(i)%val = pop%Chromosomes(iAta)%genes(i)%val
          else
            newPop%Chromosomes(iAta)%genes(i)%val = pop%Chromosomes(iMama)%genes(i)%val
          end if
        end do

      else
        ! no fun for this one
        call gaCopyChromosome(pop%Chromosomes(iAta),newPop%Chromosomes(iAta))   
      end if
    end do
  
  end subroutine
  


! ----------------------------------------------------------------------  
subroutine gaChooseFittestChromosomes(pop,newPop) 
  type(PopulationType) :: pop,newPop
  integer k,j
  real(rk) r

  do j = 1,pop%Nchrom
    r = getRandom01()
    do k = 1,pop%Nchrom
      if (r.le.pop%Chromosomes(k)%cumProb) then
        ! this chomosome is selected for the next generation
        call gaCopyChromosome(pop%Chromosomes(k),newPop%Chromosomes(j)) ! k -> j
        exit
      end if
    end do
  end do

end subroutine


! ----------------------------------------------------------------------
  subroutine gaCopyChromosome(c1,c2)   ! c1 -> c2
    type(ChromosomeType) c1,c2
    integer i

    do i=1,size(c2%genes)
      c2%genes(i)%val = c1%genes(i)%val
    end do
    c2%objF = c1%objF
    c2%fitness = c1%fitness
    c2%probability = c1%probability
    c2%cumProb = c1%cumProb

  end subroutine


! ----------------------------------------------------------------------
  subroutine gaGenerateInitialPopulation(p) 
    type(PopulationType) :: p
    integer i,j

    call gaEvaluateMutationNumber(p) 

    allocate (p%best%genes(p%Ngenes))
    allocate (p%chromosomes(p%Nchrom))

    do i=1,p%Nchrom
      allocate (p%chromosomes(i)%genes(p%Ngenes))
      do j=1,p%Ngenes
        call gaRandomizeGene(p%chromosomes(i)%genes(j))
      end do
    end do

  end subroutine


! ----------------------------------------------------------------------
  subroutine gaEvaluteCumulativeProbability(p) 
    type(PopulationType) :: p
    integer i
    real(rk) cp

    cp = 0.0_rk;
    do i=1,p%Nchrom
      p%chromosomes(i)%probability = p%chromosomes(i)%fitness / p%totalFitness
      cp = cp + p%chromosomes(i)%probability
      p%chromosomes(i)%cumProb = cp
    end do
  end subroutine



! ----------------------------------------------------------------------
  subroutine gaEvaluteTotalFintess(p) 
    type(PopulationType) :: p
    integer i

    p%totalFitness = 0.0_rk;
    do i=1,p%Nchrom
      p%totalFitness = p%totalFitness + p%chromosomes(i)%fitness;
    end do
  end subroutine


! ----------------------------------------------------------------------
  subroutine gaEvaluateMutationNumber(p) 
    type(PopulationType) :: p
    p%MutationNumber = floor(p%Nchrom * p%Ngenes * p%MutationRate)
  end subroutine

! ----------------------------------------------------------------------
  subroutine gaEvaluateFitness(c) 
    type(ChromosomeType) :: c

    c%fitness = 1.0_rk / (1.0_rk + c%objF);
  end subroutine

end module