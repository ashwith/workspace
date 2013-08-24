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

!Function to find the determinant of a square matrix
!Author: Louisda16th a.k.a Ashwith J. Rego 
!Email: ashwith@ieee.org
!Description: The subroutine is based on two key points:
!1] A determinant is unaltered when row operations are performed: Hence, using this principle,
!row operations (column operations would work as well) are used
!to convert the matrix into upper traingular form
!2]The determinant of a triangular matrix is obtained by finding the product of the diagonal elements


REAL FUNCTION FindDet(matrix, n)
  IMPLICIT NONE
  REAL, DIMENSION(n,n) :: matrix
  INTEGER, INTENT(IN) :: n
  REAL :: m, temp
  INTEGER :: i, j, k, l

  !Flag to know if the matrix is singular
  LOGICAL :: DetExists = .TRUE.
  !l stores the sign change of the matrix in case there are row exchanges
  l = 1
  
!====================================================================  
!                  Convert to upper triangular form
!====================================================================  

  !For each pivot row
  DO k = 1, n-1

    !If the pivot element is 0 then 
    !the matrix is probably singular
    !We'll need to exchange this
    !row with another one which
    !has a nonzero pivot.
    IF (matrix(k,k) == 0) THEN 
      DetExists = .FALSE.            
    
      !This DO loop searches
      !for the nearest row with
      !a non zero pivot. A better
      !way would be to look for the
      !largest non-zero pivot.
      DO i = k+1, n 
        !Check if the pivot is nonzero
        !if it is, perform the row exchange
        IF (matrix(i,k) /= 0) THEN
          !This loop does the row exchange
          DO j = 1, n 
            temp = matrix(i,j)
            matrix(i,j)= matrix(k,j)
            matrix(k,j) = temp
          END DO
          !Because we were able to find 
          !a non-zero pivot, the matrix
          !may be non-singular so set
          !the flag
          DetExists = .TRUE.
          !We'll need to change the sign
          !because of the row exchange 
          !#Possible Bug#: I think the 
          !sign change should be outside 
          !the if
          l=-l

          !Exit the outer Do loop
          !because we found a row we
          !can exchange with
          EXIT
        ENDIF
      END DO
      
      !If no row was found, the matrix is
      !singular and we can stop here.
      IF (DetExists .EQV. .FALSE.) THEN
        FindDet = 0
        return
      END IF
      
    ENDIF
    
    !This Do loop performs the row operations
    !as per the Gauss elimination algorithm.
    DO j = k+1, n
      !Find the correct multiple for the current row
      m = matrix(j,k)/matrix(k,k)

      !Subtract the pivot row from the current row.
      DO i = k+1, n
        matrix(j,i) = matrix(j,i) - m*matrix(k,i)
      END DO
    END DO
  END DO
!====================================================================  
  
!====================================================================  
!     Calculate determinant by finding product of diagonal elements
!====================================================================  
  FindDet = l
  DO i = 1, n
    FindDet = FindDet * matrix(i,i)
  END DO
  
END FUNCTION FindDet
