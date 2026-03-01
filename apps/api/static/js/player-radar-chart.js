document.addEventListener('DOMContentLoaded', () => {
  const canvas = document.getElementById('playerRadarChart');
  if (!canvas) return;
  const ctx = canvas.getContext('2d');
  const rawData = canvas.dataset.chartData;
  if (!rawData) return;

  try {
    const parsedData = JSON.parse(rawData);
    // Dark mode check
    const isDark = document.documentElement.classList.contains('dark');
    const textColor = isDark ? '#94a3b8' : '#64748b';
    const gridColor = isDark ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)';
    const pointBg = isDark ? '#f97316' : '#c2410c'; // orange
    const fillBg = isDark ? 'rgba(249, 115, 22, 0.2)' : 'rgba(194, 65, 12, 0.2)';

    new Chart(ctx, {
      type: 'radar',
      data: {
        labels: parsedData.labels,
        datasets: [{
          label: parsedData.datasetLabel,
          data: parsedData.data,
          backgroundColor: fillBg,
          borderColor: pointBg,
          pointBackgroundColor: pointBg,
          pointBorderColor: '#fff',
          pointHoverBackgroundColor: '#fff',
          pointHoverBorderColor: pointBg,
          borderWidth: 2,
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        scales: {
          r: {
            angleLines: { color: gridColor },
            grid: { color: gridColor },
            pointLabels: {
              color: textColor,
              font: { family: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace', size: 11 }
            },
            ticks: { display: false, max: parsedData.maxScale || undefined }
          }
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: isDark ? '#1e293b' : '#fff',
            titleColor: isDark ? '#f8fafc' : '#0f172a',
            bodyColor: isDark ? '#cbd5e1' : '#334155',
            borderColor: isDark ? '#334155' : '#e2e8f0',
            borderWidth: 1,
            bodyFont: { family: 'ui-monospace' }
          }
        }
      }
    });
  } catch(e) {
    console.error('Failed to initialize radar chart', e);
  }
});
