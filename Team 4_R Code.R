#Team 4: Chrizenn Gamayo, Ismael Ilboudo, Yvonne Avugwi, and Katherine Prieto

# Part 1

# TV exposures x[1], Social Media exposures x[2], Newspaper exposures x[3] # Define variables

f=function(x) -(1.3*x[1]+0.6*x[2]+0.5*x[3]) # Define the objective function. Note:to maximize, we must minimize -f

Equalities=function(x){     # Define the equality constraints using artificial variables approach
  h=0
  h[1]=0.04*x[2]+0.12*x[3]-1.49
  h[2]=x[4]
  h[3]=x[5]
  h[4]=x[6]
  h[5]=x[7]
  h[6]=x[8]
  
  return(h)}

Inequalities=function(x){     # Define the inequality constraints using artificial variables approach
  h=0
  h[1]=5-x[1]
  h[2]=4-0.3*x[1]-0.15*x[2]-0.1*x[3]
  h[3]=1-0.09*x[1]-0.03*x[2]-0.04*x[3]
  h[4]=1.2*x[1]+0.2*x[2]-5+x[4]
  h[5]=0.5*x[1]+0.2*x[2]+0.2*x[3]-5+x[5]
  h[6]=x[1]+x[6]
  h[7]=x[2]+x[7]
  h[8]=x[3]+x[8]
  return(h)}

p0=c(0,0,0,6,6,1,1,1)  # Give an initial point

y=constrOptim.nl(p0,f,heq=Equalities, hin=Inequalities); # Run the optimization

print(y$par)
print(y$value)


# Part 2 

# x[1] = TV, x[2] = Social Media, x[3] = News

# Relationship between the # of TV commercials (TV) on the # of first-time customer visits (S)
# S1 = -0.1*TV^2 + 1.13*TV - 0.04

# Relationship between the # of social media ads (M) on the # of first-time customer visits (S)
# S2 = -0.002*M^2 + 0.124*M + 0.14

# Relationship between the # of newspaper ads (N) on the # of first-time customer visits (S)
# S3 = -0.0321*N^2 + 0.706*N - 0.09

# Advertising Cost
# 0.3*TV-0.15*M-0.1*N

# Planning Cost
# 0.09*TV-0.03*M-0.04*N

# Total Profit: Profit(excluding advertising and planning costs)*customers - (advertising and planning costs)*customers

(f=function(x) -((5*(-0.1*x[1]^2 + 1.13*x[1] - 0.04)) + (5*(-0.002*x[2]^2 + 0.124*x[2] + 0.14)) + (5*(-0.0321*x[3]^2 + 0.706*x[3] - 0.09)) 
                 - (0.3*x[1]-0.15*x[2]-0.1*x[3])- (0.09*x[1]-0.03*x[2]-0.04*x[3]))) # Maximize Total Profit function

Equalities=function(x){     # Define the equality constraints using artificial variables approach
  h=0
  h[1]=0.04*x[2]+0.12*x[3]-1.49
  return(h)}

Inequalities=function(x){     # Define the inequality constraints using artificial variables approach
  h=0
  h[1]=5-x[1]
  h[2]=4-0.3*x[1]-0.15*x[2]-0.1*x[3]
  h[3]=1-0.09*x[1]-0.03*x[2]-0.04*x[3]
  h[4]=1.2*x[1]+0.2*x[2]-5
  h[5]=0.5*x[1]+0.2*x[2]+0.2*x[3]-5
  h[6]=x[1]
  h[7]=x[2]
  h[8]=x[3]
  return(h)}

p0=c(2.31, 17.68, 6.52)  # Give an initial point

y=constrOptim.nl(p0,f,heq=Equalities, hin=Inequalities); 

print(y$par)
print(y$value) # Multiply by -1 since we are minimizing -f
