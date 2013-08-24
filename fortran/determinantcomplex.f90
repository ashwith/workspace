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


PROGRAM MAT_DET
  IMPLICIT NONE
  
  COMPLEX, ALLOCATABLE, DIMENSION(:,:) :: Matrix
  COMPLEX :: Determinant, FindDet
  INTEGER :: n, i, j
  
  PRINT*,"Enter order of matrix"
  READ*, n
  
  ALLOCATE(Matrix(n,n))
  
  PRINT*,"Enter elements row-wise"
  
  DO i = 1,n
    READ*,(MATRIX(i,j),j=1,n)
  END DO
  
  Determinant = FindDet(Matrix,n)
  
  PRINT*,"Determinant =",Determinant
END PROGRAM MAT_DET

!Function to find the determinant of a square matrix
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description: The subroutine is based on two key points:
!1] A determinant is unaltered when row operations are performed: Hence, using this principle,
!row operations (column operations would work as well) are used
!to convert the matrix into upper traingular form
!2]The determinant of a triangular matrix is obtained by finding the product of the diagonal elements
!
COMPLEX FUNCTION FindDet(matrix, n)
  IMPLICIT NONE
  COMPLEX, DIMENSION(n,n) :: matrix
  INTEGER, INTENT(IN) :: n
  COMPLEX :: m, temp
  INTEGER :: i, j, k, l
  LOGICAL :: DetExists = .TRUE.
  l = 1
  !Convert to upper triangular form
  DO k = 1, n-1
    IF (matrix(k,k) == 0) THEN
      DetExists = .FALSE.
      DO i = k+1, n
        IF (matrix(i,k) /= 0) THEN
          DO j = 1, n
            temp = matrix(i,j)
            matrix(i,j)= matrix(k,j)
            matrix(k,j) = temp
          END DO
          DetExists = .TRUE.
          l=-l
          EXIT
        ENDIF
      END DO
      IF (DetExists .EQV. .FALSE.) THEN
        FindDet = 0
        return
      END IF
    ENDIF
    DO j = k+1, n
      m = matrix(j,k)/matrix(k,k)
      DO i = k+1, n
        matrix(j,i) = matrix(j,i) - m*matrix(k,i)
      END DO
    END DO
  END DO
  
  !Calculate determinant by finding product of diagonal elements
  FindDet = l
  DO i = 1, n
    FindDet = FindDet * matrix(i,i)
  END DO
  
END FUNCTION FindDet
  
