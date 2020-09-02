init.lstm = function(input.size, hidden.size, output.size){
  #calculate number of weights at each gate
  n.weights = (input.size + hidden.size)*hidden.size
  
  gates = list(
    activation = list(
      W = matrix(rnorm(n.weights), nrow=hidden.size),
      b = matrix(rnorm(hidden.size), nrow=hidden.size)
    ),
    input = list(
      W = matrix(rnorm(n.weights), nrow=hidden.size),
      b = matrix(rnorm(hidden.size), nrow=hidden.size)
    ),
    forget = list(
      W = matrix(rnorm(n.weights), nrow=hidden.size),
      b = matrix(rnorm(hidden.size), nrow=hidden.size)
    ),
    output = list(
      W = matrix(rnorm(n.weights), nrow=hidden.size),
      b = matrix(rnorm(hidden.size), nrow=hidden.size)
    ),
    predict = list(
      W = matrix(rnorm(hidden.size), ncol=hidden.size),
      b = matrix(rnorm(output.size), ncol=input.size)
    )
  )
  return(list(gates=gates, input.size=input.size, output.size=output.size, hidden.size=hidden.size))
}

forward.pass = function(X, lstm){
  #obtain lists containing the weights and biases at each gate of the lstm
  gates = lstm$gates
  input = gates$input
  activation = gates$activation
  forget = gates$forget
  output = gates$output
  predict = gates$predict
  
  #obtain dimensions of lstm
  hidden.size = lstm$hidden.size
  input.size = lstm$input.size
  output.size = lstm$output.size
  
  #initialize storage
  N = length(X)
  
  h = matrix(0, nrow=hidden.size)
  C = matrix(0, nrow=hidden.size)

  for (i in 1:N){
    print(h)
    print(C)
    xh = rbind(X[,i], h)
    #calculate value at forget gate
    f = sigmoid(forget$W%*%xh+forget$b)
    #calculate value at input gate
    i = sigmoid(input$W%*%xh+input$b)
    #calculate input activation
    a = tanh(activation$W%*%xh+activation$b)
    #update the internal cell state
    C = f*C + i*a
    #calculate value at output gate
    o = sigmoid(output$W%*%xh+output$b)
    #update hidden state
    h = o*tanh(C)
    #get prediction as a linear combination of hidden state
    p = predict$W%*%h + predict$b
    print(p)
  }
}

lstm = init.lstm(1, 3, 1)


X = matrix(rnorm(5), nrow=1)

forward.pass(X, lstm)
