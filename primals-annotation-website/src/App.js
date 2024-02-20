import React, { useState } from 'react';
import data from './data.json'; // Import the JSON data

function App() {
  const [selectedOption, setSelectedOption] = useState(null);

  const handleOptionChange = (option) => {
    setSelectedOption(option);
  };

  return (
    <div>
      <header>
        <h1>{data.title}</h1>
        <p>{data.content}</p>
      </header>
      <main>
        <div>
          <label>
            <input
              type="radio"
              value="good"
              checked={selectedOption === "good"}
              onChange={() => handleOptionChange("good")}
            />
            Good
          </label>
          <label>
            <input
              type="radio"
              value="bad"
              checked={selectedOption === "bad"}
              onChange={() => handleOptionChange("bad")}
            />
            Bad
          </label>
        </div>
        <div>
          <h2>Tree Structure</h2>
          {/* Tree structure goes here */}
        </div>
      </main>
    </div>
  );
}

export default App;
