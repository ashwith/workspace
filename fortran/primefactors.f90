!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


PROGRAM FINDFACT
  IMPLICIT NONE
  INTEGER :: n
  INTEGER, ALLOCATABLE, DIMENSION(:) :: factors
  INTEGER :: i, f
  
  PRINT*, "Enter a number"
  READ*,n
  
  ALLOCATE(factors(n/2))
  
  CALL PRIMEFACTORS(n,factors,f)
  
  PRINT*,n, " = 1 ",(" x ",factors(i),i=1,f)
END PROGRAM FINDFACT

!SUBROUTINE TO FIND THE PRIME FACTORS OF A NUMBER
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description: 
!Algorithm is quite easy:
!Start with 2, check whether 2 is a factor by seeing if MOD(<input_number>,2)
!is zero. If it is zero, then 2 becomes a factor. If not, check with the next number.
!When a factor is found, divide the given number with the factor found. However,
!donot move to the next possible factor - a number can occur more than once as a factor
SUBROUTINE PRIMEFACTORS(num, factors, f)
!Declarations
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: num  !input number
  INTEGER,INTENT(OUT), DIMENSION((num/2))::factors !Array to store factors
  INTEGER, INTENT(INOUT) :: f
  INTEGER :: i, n
  i = 2  !Eligible factor
  f = 1  !Number of factors
  n = num !store input number into a temporary variable
  DO
    IF (MOD(n,i) == 0) THEN !If i divides 2, it is a factor
      factors(f) = i
      f = f+1
      n = n/i
    ELSE
      i = i+1     !Not a factor. Move to next number
    END IF
    IF (n == 1) THEN    
               !Since f is incremented after a factor is found
      f = f-1    !its value will be one more than the number of factors
          !Hence the value of f is decremented
      EXIT
    END IF
  END DO
END SUBROUTINE PRIMEFACTORS
