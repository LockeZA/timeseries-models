#sim data
X = rep(c(1,2), 10)
y = X[2:N]
X = head(X, N-1)
X = matrix(X, ncol=1)

lstm = init.lstm(1,1)

fit = lstm.fit(y, X, lstm)

lstm.forward.pass(X, fit$LSTM, 1)$yhat

fit$LSTM
lstm