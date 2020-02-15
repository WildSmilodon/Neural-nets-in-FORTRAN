module mTrainingData
  use mCommon
  implicit none

contains

!  -------------------------------------------------------------------------------------------------
type(TrainingDataType) function tdCreate(n,inpLength,outLength,tdType,tdXtype)
!
!  creates training data set
!
! -------------------------------------------------------------------------------------------------
  type(TrainingDataType) :: td
  integer n,inpLength,outLength,tdType,tdXtype
  integer i

  td%n = n
  td%inpLength = inpLength
  td%outLength = outLength

  allocate (td%x(td%inpLength,td%n))
  allocate (td%y(td%outLength,td%n))

! input values
  select case (tdXtype)
 
    case (tdXlin) 
      do i=1,td%n
        td%x(1,i) = real(i)/real(td%n) 
      end do

    case (tdXrnd) 
      do i=1,td%n
        td%x(1,i) = getRandom01()
      end do

    case default
        print *, "ERROR" 
  
  end select

! output values  
  select case (tdType)

    case (tdSin) 
      do i=1,td%n
        td%y(1,i) = sin(4.0_rk*pi*td%x(1,i)) + 4.0_rk * td%x(1,i)
      end do

    case (tdSinNoise) 
      do i=1,td%n
        td%y(1,i) = sin(4.0_rk*pi*td%x(1,i)) + 4.0_rk * td%x(1,i)
        td%y(1,i) = td%y(1,i) +  0.5_rk * ( getRandom01() - 0.5_rk ) 
      end do

    case (tdSin2) 
      do i=1,td%n
        td%y(1,i) = sin(td%x(1,i))
      end do

    case (tdCos) 
      do i=1,td%n
        td%y(1,i) = cos(td%x(1,i))
      end do

    case (tdLin) 
      do i=1,td%n
        td%y(1,i) = td%x(1,i)
      end do

    case (tdPar) 
      do i=1,td%n
        td%y(1,i) = td%x(1,i)**2.0_rk
      end do

    case (tdParSin) 
      do i=1,td%n
        td%y(1,i) = ( 1.0_rk - ( td%x(1,i) - 0.5_rk )**2.0_rk ) + 0.1_rk * sin(4.0_rk*pi*td%x(1,i)) 
      end do
      

    case default
        print *, "ERROR" 
    
  end select

  tdCreate = td

end function


!
! -------------------------------------------------------------------------------------------------
!
real(rk) function getRandom01()
  real(rk) r
  call random_number(r)
  getRandom01 = r
end function


end module
