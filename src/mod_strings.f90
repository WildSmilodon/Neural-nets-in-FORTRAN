      MODULE String_Utility 
      IMPLICIT NONE 
      PRIVATE 
      PUBLIC :: StrLowCase 

   ! List of character for case conversion 
      CHARACTER(*), PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz' 
      CHARACTER(*), PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 

      CONTAINS 

      FUNCTION StrLowCase( Input_String ) RESULT( Output_String ) 
        CHARACTER(*), INTENT(IN)     :: Input_String 
        CHARACTER(LEN(Input_String)) :: Output_String 
        INTEGER :: i, n 

        ! Copy input string 
        Output_String = Input_String 

        ! Convert case character by character 
        DO i = 1, LEN(Output_String) 
          n = INDEX(UPPER_CASE, Output_String(i:i)) 
          IF ( n /= 0 ) Output_String(i:i) = LOWER_CASE(n:n) 
        END DO 
      END FUNCTION StrLowCase 

      END MODULE String_Utility 


!______________________________________________________________________C
!______________________________________________________________________C
      SUBROUTINE rOneTL(lun,OneLine)
!     _    ___ _    _ 
!     Read One Text Line 
!
!______________________________________________________________________C
!     Returns the first nonempty text line in file LUN, which does not
!     include the # character. If end of file is encoutered, it returns EOF
      CHARACTER*(*) OneLine
      INTEGER lun,i
  
10    READ(lun,'(A)',END=20) OneLine  

!     Check if line is empty
      IF (len_trim(OneLine).EQ.0) GOTO 10

!     Check if line contains # character
      DO i=1,len_trim(OneLine)
        IF (OneLine(i:i).EQ.'#') GOTO 10
      ENDDO

      RETURN

20    OneLine='EOF'
      END           
                  