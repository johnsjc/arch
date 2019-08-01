## Author: Jc J
## Date: July 26, 2019
##
## Debugging functions for mergesort program
##

.globl debug.bubble_1

.globl debug.recursive_1
.globl debug.recursive_2
.globl debug.recursive_3
.globl debug.recursive_4

.globl debug.iterative_1
.globl debug.iterative_2
.globl debug.iterative_3

.globl debug.newline

.data

debug_pass:			.asciiz "[DEBUG] Pass: "
debug_swap:			.asciiz "[DEBUG] Swap: "
debug_result:		.asciiz "[DEBUG] Result: "
debug_noswaps:		.asciiz "[DEBUG] No swaps necessary.\n"
debug_sort:         .asciiz "[DEBUG] Sort: "
debug_split:        .asciiz "[DEBUG] Split: "
debug_left:         .asciiz "[DEBUG] Sorted left half: "
debug_right:        .asciiz "[DEBUG] Sorted right half: "
debug_merge:        .asciiz "[DEBUG] Merged result: "
debug_size:         .asciiz "[DEBUG] Size: "
debug_delim:  		.asciiz " | "

.text

## bubble_1
##
## Prints the current pass
## e.g. [DEBUG] Pass 1:
##
##
debug.bubble_1:

		subu		$sp, $sp, 32
		sw			$s0, 28($sp)
		
		move		$s0, $a0
						
		la			$a0, debug_pass							# print [DEBUG] Pass:
		li			$v0, 4
		syscall
		
		lw			$a0, 32($sp)							# print the current pass
		li			$v0, 1
		syscall
		
		li			$a0, 0xA								# print a new line
		li			$v0, 11
		syscall
		
		lw			$s0, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of bubble_1


## bubble_2
##
## Prints the list to be sorted
## e.g. [DEBUG] Sort: [3, 2, 1, 4, 5]
##
debug.bubble_2:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		li			$a0, 0x9
		li			$v0, 11
		syscall
		
		la			$a0, debug_sort							# print [DEBUG] Sort:
		li			$v0, 4
		syscall
		
		li			$a0, 0x9
		li			$v0, 11
		syscall
		
		lw			$a0, 40($sp)							# print the list
		jal			util.print_list
		
		li			$a0, 0xA								# print a new line
		li			$v0, 11
		syscall
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of bubble_2


## bubble_3
##
## Prints the swap
## e.g. [DEBUG] Swap: [3] | [2]
##
## Arguments:
##		a0:		index i-1
##		a1:		index i
##
debug.bubble_3:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)		
		sw			$s0, 24($sp)
		sw			$s1, 20($sp)
		
		move		$s0, $a0								# s0: start (i - 1)
		move		$s1, $a1								# s1: end (i)
		
		li			$a0, 0x9
		li			$v0, 11
		syscall
		
		
		la			$a0, debug_swap							# print [DEBUG] Swap:
		li			$v0, 4
		syscall
		
		li			$a0, 0x9
		li			$v0, 11
		syscall
		
		lw			$a0, 40($sp)							# print list[i-1:i]
		move		$a1, $s0
		move		$a2, $s1
		jal			util.print_list_range
		
		la			$a0, debug_delim						# print |
		li			$v0, 4
		syscall
		
		lw			$a0, 40($sp)							# print list[i:i+1]
		move		$a1, $s1
		addi		$a2, $s1, 1
		jal			util.print_list_range
		
		li			$a0, 0xA								# print a new line
		li			$v0, 11
		syscall
		
		li			$a0, 0x9
		li			$v0, 11
		syscall
		
		lw			$s0, 24($sp)
		lw			$s1, 20($sp)
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of bubble_3


## bubble_4
##
## Prints the result of a swap.
## e.g.	[3] and [2] were swapped
## [DEBUG] Result: [2, 3, 1, 4, 5]
##
##
debug.bubble_4:
		
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)			

		la			$a0, debug_result						# print [DEBUG] Result: 
		li			$v0, 4
		syscall
		
		lw			$a0, 40($sp)							# print the list
		jal			util.print_list
		
		li			$a0, 0xA								# print two new lines
		li			$v0, 11
		syscall
		syscall
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of bubble_4


## bubble_5
##
## Prints [DEBUG] No swaps necessary\n.
##
##
debug.bubble_5:

		subu		$sp, $sp, 32
		
		la			$a0, debug_noswaps						# print [DEBUG] No swaps necessary.
		li			$v0, 4
		syscall
		li			$a0, 0xA
		li			$v0, 11
		syscall
		
		addu		$sp, $sp, 32
		jr			$ra
## end of bubble_5


## recursive_1
##
## Prints the list that is currently being sorted.
## e.g. [DEBUG] Sort: [3, 2, 1, 4, 5]
##
debug.recursive_1:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)

		li			$a0, 0x9								# indent according to recursion depth
		lw			$a1, 44($sp)							# ascii 0x9 is a tab character
		jal			util.print_char_n_times
		
		la			$a0, debug_sort							# print "[DEBUG] Sort: "
		li			$v0, 4
		syscall
		
		lw			$a0, 56($sp)							# print list[start:end]
		lw			$a1, 52($sp)
		lw			$a2, 48($sp)
		jal			util.print_list_range
		
		li			$a0, 0xA								# print a new line
		li			$v0, 11
		syscall
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32		
		jr			$ra
## end of recursive_1


## recursive_2
##
## Prints the two halves of the list
## e.g. ## [DEBUG] Split: [3, 2] | [1, 4, 5]
##
debug.recursive_2:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)

		li			$a0, 0x9								# indent according to recursion depth
		lw			$a1, 44($sp)							# ascii 0x9 is a tab character
		jal			util.print_char_n_times
		
		
        la      	$a0, debug_split                        # print "[DEBUG] Split: "
        li      	$v0, 4
        syscall
        
        lw			$a0, 56($sp)							# print list[start:middle]
		lw			$a1, 52($sp)
		lw			$a2, 40($sp)
		jal			util.print_list_range
		
		la      	$a0, debug_delim                  # print  " | "
        li      	$v0, 4
        syscall
				
		lw			$a0, 56($sp)							# print list[middle:end]
		lw			$a1, 40($sp)
		lw			$a2, 48($sp)
		jal			util.print_list_range

        li      	$a0, 0xA                                # print a new line
        li      	$v0, 11
        syscall
        
        lw			$ra, 28($sp)
        addu		$sp, $sp, 32       
        jr			$ra
## end of recursive_2


## recursive_3
##
## Prints the two sorted halves of the list
## e.g. [DEBUG] Sorted left half: [1, 3]
## 		[DEBUG] Sorted right half: [2, 5]
debug.recursive_3:

        subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		li			$a0, 0x9								# indent according to recursion depth
		lw			$a1, 44($sp)							# ascii 0x9 is a tab character
		jal			util.print_char_n_times
		
		la      	$a0, debug_left                         # print "[DEBUG] Sorted left half: "
        li      	$v0, 4
        syscall

        lw      	$a0, 56($sp)                            # print list[start:middle]
        lw	     	$a1, 52($sp)                            
        lw     		$a2, 40($sp)                            
        jal     	util.print_list_range                        

        li      	$a0, 0xA                                # print a new line
        li      	$v0, 11
        syscall
        
    	li			$a0, 0x9								# indent according to recursion depth
		lw			$a1, 44($sp)							# ascii 0x9 is a tab character
		jal			util.print_char_n_times
		
		la      	$a0, debug_right                       	# print "[DEBUG] Sorted right half: "
        li      	$v0, 4
        syscall
        
        lw      	$a0, 56($sp)                            # print list[middle:end]
        lw      	$a1, 40($sp)                            
        lw      	$a2, 48($sp)                            
        jal     	util.print_list_range     
        
        li      	$a0, 0xA                                # print a new line
        li      	$v0, 11
        syscall         
        
        lw			$ra, 28($sp)
        addu		$sp, $sp, 32       
        jr			$ra 
## end of recursive_3


## recursive_4
##
## Prints the merged result
## e.g. [DEBUG] Merged result: [2, 3]
debug.recursive_4:

        subu		$sp, $sp, 32
		sw			$ra, 28($sp)

		li			$a0, 0x9								# indent according to recursion depth
		lw			$a1, 44($sp)							# ascii 0x9 is a tab character
		jal			util.print_char_n_times
		
		la      	$a0, debug_merge                        # print "[DEBUG] Merged result: "
        li      	$v0, 4
        syscall

        lw      	$a0, 56($sp)                            # print list[start:end]
        lw      	$a1, 52($sp)                            
        lw      	$a2, 48($sp)                            
        jal     	util.print_list_range                          
        
        li      	$a0, 0xA                                # print a new line
        li      	$v0, 11
        syscall                   

        lw			$ra, 28($sp)
        addu		$sp, $sp, 32       
        jr			$ra 
## end of recursive_4


## iterative_1
##
## Prints the size of the sublists in the current iteration
##
## Arguments:
##		a0:		size
##
## Register usage:
##		s0:		size
##
debug.iterative_1:

		subu		$sp, $sp, 32
		sw			$s0, 28($sp)		
		move		$s0, $a0

        la      	$a0, debug_size                     	# print "[DEBUG] Size: "
        li      	$v0, 4
        syscall

        move    	$a0, $s0                            	# print size
        li      	$v0, 1
        syscall

        li      	$a0, 0xA                            	# print a new line
        li      	$v0, 11
        syscall
        
        lw			$s0, 28($sp)
        addu		$sp, $sp, 32 		
 		jr			$ra
## end of iterative_1


## iterative_2
##
## Prints the sublists
##
## Register usage:
##		s0:		middle
##		s1:		end
##
debug.iterative_2:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)	
		
		sw			$s0, 24($sp)
		sw			$s1, 20($sp)
		
		li			$a0, 0x9
		li			$v0, 11
		syscall	

        la      	$a0, debug_split                     	# print "[DEBUG] Split: "
        li      	$v0, 4
        syscall
        
        lw			$a0, 64($sp)							# print list[start:middle]
        lw			$a1, 60($sp)
        lw			$a2, 52($sp)
        jal			util.print_list_range
        
        lw			$s0, 52($sp)							# s0: middle
        lw			$s1, 56($sp)							# s1: end
        beq			$s0, $s1, di2_skip						# skip empty lists in the output
        
        la      	$a0, debug_delim                 		# print " | "
        li      	$v0, 4
        syscall
        
        lw			$a0, 64($sp)							# print list[middle:end]
        lw			$a1, 52($sp)
        lw			$a2, 56($sp)
        jal			util.print_list_range
        
    di2_skip:
    
    	li      	$a0, 0xA                            	# print a new line
        li      	$v0, 11
        syscall
        
        lw			$s0, 24($sp)
		lw			$s1, 20($sp)	
        
        lw			$ra, 28($sp)
        addu		$sp, $sp, 32 		
 		jr			$ra
## end of iterative_2

## iterative_3
##
## Prints the merged result
##
debug.iterative_3:

		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		li			$a0, 0x9
		li			$v0, 11
		syscall

        la      	$a0, debug_merge                     	# print "[DEBUG] Merged result: " 
        li      	$v0, 4
        syscall
        
        lw			$a0, 64($sp)							# print list[start:end]
        lw			$a1, 60($sp)
        lw			$a2, 56($sp)
        jal			util.print_list_range
        
        li      	$a0, 0xA                            	# print two new lines
        li      	$v0, 11
        syscall
        syscall
        
        lw			$ra, 28($sp)
        addu		$sp, $sp, 32 		
 		jr			$ra
## end of iterative_3


## newline
##
## Prints a new line
##
debug.newline:

		li      	$a0, 0xA                                # print a new line
        li      	$v0, 11
 		syscall
 		jr			$ra
## end of newline
