# ICA2

## Description

This `ica2` Clojure project, is designed for analyzing and optimizing the sales of travel tickets. The application processes data to determine the most cost-effective travel routes for groups and individuals, catering to specific travel needs based on a variety of factors such as price, number of connections, and passenger composition (families or groups).

## Installation

Ensure you have Clojure and Leiningen installed on your system for running the project.

## Usage

Run the application using Leiningen:

```bash
$ lein run
Simulation completed!
Sold tickets: 1053 piece(s)
Earned: 636300
```
This command initiates the ticket selling simulation based on the provided data.
## Examples
If you want to test hypothesis, you can change these lines here: `core.clj:99`
```
            max-price (* 0.7 (get-in group-analysis-data [:price-stats :max]))
            max-connections (get-in group-analysis-data [:flights-stats :max])
```

## Core Components
### calculate.clj
Handles the main logic for data analysis, including groupings by route, price, and passenger types (families or groups). It offers functions for extracting, grouping, and analyzing passenger data to aid in route planning and price optimization.

### core.clj
Contains the main functionality for route finding and travel plan preparation. It uses the data analyzed in calculate.clj to create travel plans that are cost-effective and meet the requirements of the passengers.

### project.clj
Configures the Clojure project, including dependencies and build settings. It ensures the correct environment is set up for running and compiling the application.

### sales_routines.clj and broker.clj
Work together to simulate the ticket selling process. broker.clj reads input data and calls functions from sales_routines.clj to determine if a sale should be made based on the proposed travel plan and budget constraints.

## Options
Currently, there are no configurable options exposed for command-line usage. All configurations are internal within the code.

### Bugs
If you run `lein run` you will see error output in the end:
```
...
Execution error at user/eval140 (form-init15072442974051940997.clj:1).
Cannot find anything to run for: ica2.sales_routines

Full report at:
/var/folders/9h/tk9j9_w50mnftnkk36lnk1k00000gn/T/clojure-18306426010148915406.edn
```

### License
This software is provided under the Eclipse Public License 2.0, available at http://www.eclipse.org/legal/epl-2.0, with a secondary license option under the GNU General Public License as published by the Free Software Foundation, version 2 or later, with the GNU Classpath Exception available at https://www.gnu.org/software/classpath/license.html.