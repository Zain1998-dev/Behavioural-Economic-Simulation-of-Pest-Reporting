# Behavioural-Economic Simulation of Pest Reporting and Phytosanitary Policy

This project develops a simple simulation model in R to explore how different pest management policies influence farmers' reporting behaviour and economic outcomes.

The model generates a synthetic dataset of 200 farms with different characteristics, including farm size, crop type, pest risk, prevention costs, and risk preferences. Farmers decide whether to report pest outbreaks based on economic incentives, monitoring systems, and behavioural factors.

## Policy Scenarios

The model evaluates four policy options:

- Baseline (No compensation, No monitoring)
- Compensation Only
- Monitoring Only
- Combined Compensation and Monitoring

## Outputs

The simulation produces:

- Reporting rates under each policy scenario
- Total pest losses
- Government costs
- Social costs
- Regression results
- Policy comparison figures

## Project Structure

```text
scripts/
├── main_simulation.R

outputs/
├── figures/
├── tables/

report/
├── project_note.pdf
