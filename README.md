# 🏆 Fantasy Premier League (FPL) Data Dashboard

An interactive **R Shiny web application** for visualising and analysing Fantasy Premier League (FPL) data.
This dashboard provides detailed insights into player performance, team trends, transfers, ownership, and mini-league comparisons — all in one place.

The app has been deployed to *shinyapps.io* and can be found [here](https://andypetes.shinyapps.io/FPL_App2/)

---

## 🚀 Features

### 🔹 Player Comparison

* Compare players across multiple statistics (Goals, Assists, xG, xA, Value, etc.)
* Filter by position, price, and gameweeks
* Generate interactive scatter plots and bar charts
* Download plots as PNG or HTML

### 🔹 Rankings

* Track your team’s rank and total points over time
* Visualise rank progression and key milestones

### 🔹 Bench & Captaincy

* Analyse points from bench and captain selections
* Identify wasted points and optimal captain choices

### 🔹 Transfers

* Review historical transfer performance
* Visualise transfer gains/losses and cumulative points impact

### 🔹 Mini-League Data

* Compare your team’s performance with rivals in private leagues
* Create bump charts for rank trajectories across gameweeks

### 🔹 Fixtures

* View fixture difficulty and team strength (overall, attack, defence)
* Customise colours and gameweek ranges

### 🔹 Ownership

* Visualise player ownership across leagues and positions
* Compare rival ownership trends for strategic insights

### 🔹 Team Data

* Explore team-level metrics (xG, xA, Points, Minutes, etc.)
* Generate trend plots and heatmaps by gameweek

---

## 🧩 Technology Stack

| Component          | Description                                                                              |
| ------------------ | ---------------------------------------------------------------------------------------- |
| **Language**       | R                                                                                        |
| **Framework**      | Shiny, shinydashboard, bslib                                                             |
| **Styling**        | Custom theme via `create_theme()` using AdminLTE colours                                 |
| **Visualisations** | ggplot2, plotly, DT tables                                                               |
| **APIs**           | [Fantasy Premier League API](https://fantasy.premierleague.com/api/) for historical data |
| **Data**           | Local CSVs for processed data (`Players_History.csv`, `Fixtures.csv`, etc.)              |

---

## 🎨 Custom Theme

The app uses a custom AdminLTE theme:

* **Header colour:** `#1D3557`
* **Sidebar:** `#457B9D` (main), `#A8DADC` (hover)
* **Text:** `#F1FAEE`
* **Backgrounds:** white (`#FDFFFC`), soft blue boxes (`#EBFAFA`)

This gives the app a modern, FPL-inspired aesthetic.

---

## 📂 Data Inputs

The app reads pre-processed CSV files from the `/25_26/` folder:

```
Players_Gameweek.csv  
Players_History.csv  
General_Info.csv  
Fixtures.csv  
Team_Summary.csv
```

### 🔄 Updating Data

You can update these CSVs using the **`fpl_data_update.R`** script. This script downloads the latest data from the FPL API, processes player history, gameweek aggregates, and fixtures, then saves the outputs to `/25_26/` (or a custom folder).

---

## ⚙️ `fpl_data_update.R` Script

### Purpose

* Automatically fetches the latest FPL data for players, teams, and fixtures.
* Generates processed CSV files ready for use in the Shiny dashboard.
* Includes progress bars to track download and processing status.

### Outputs

1. `General_Info.csv` – General player info including team and position.
2. `Players_History.csv` – Player history with statistics, xG, xA, and team strength metrics.
3. `Players_Gameweek.csv` – Aggregated player performance by gameweek (SGW/DGW/BGW).
4. `Fixtures.csv` – Fixture data including home/away and strength metrics.

### Usage

Run the script from the command line:

```bash
Rscript fpl_data_update.R
```

Optional argument:

* `--output /path/to/folder` — specify a custom folder for the CSV outputs.
  Defaults to `25_26/` if not provided.

Example:

```bash
Rscript fpl_data_update.R --output ./data
```

**Notes:**

* The script creates necessary columns and ensures compatibility with the Shiny app.
* Vectorised operations improve speed, and progress bars show each download stage.

---

## ⚙️ How to Run the App

1. Clone this repository:

   ```bash
   git clone https://github.com/yourusername/FPL_App.git
   cd FPL_App
   ```

2. Open the project in **RStudio**.

3. Install required packages (if not already installed):

   ```r
   install.packages(c("shiny", "shinydashboard", "bslib", "plotly", "DT", "httr", "jsonlite", "dplyr", "tidyr"))
   ```

4. Update data (optional but recommended):

   ```bash
   Rscript fpl_data_update.R
   ```

5. Run the app:

   ```r
   shiny::runApp("app.R")
   ```

---

## 🧠 Example Insights

* Identify undervalued players based on **xG vs Goals**.
* Track your **rank progression** compared to rivals.
* Discover **fixture difficulty trends** for planning future transfers.
* Analyse **ownership** to gain a competitive edge.

---

## 🌐 Deployment

You can host this app locally via `runApp()`

---

## 👨‍💻 Author

**Andrew Peters**
*Data Scientist & Visualiser*
Transforming complex football data into clear, actionable insights.

* 🌍 [Portfolio Website](https://andypetes94.github.io/portfolio-andrewpeters/)
* 🐦 [Twitter](https://x.com/data_vizard_)
* 💼 [LinkedIn](http://linkedin.com/in/andrew-peters-phd-70b58292)
* 📊 [GitHub](https://github.com/andypetes94)

---

## 📜 License

This project is licensed under the **MIT License** — you’re free to use, modify, and share it with attribution.

---

## ⭐ Acknowledgements

* [Fantasy Premier League API](https://fantasy.premierleague.com/)
* R community & Shiny developers
* All data © Premier League, used for non-commercial, analytical purposes