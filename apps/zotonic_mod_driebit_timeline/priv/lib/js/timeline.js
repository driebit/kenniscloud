function setupTimeline() {
    if (window.innerWidth < 800) {
        return;
    }

    const items = document.querySelectorAll('.c-timeline__item');
    const timeline = document.querySelector('.c-timeline');
    const timelineLine = document.querySelector('.c-timeline__line');

    // Check if required elements exist
    if (!timeline || !timelineLine || items.length === 0) {
        console.warn('Timeline elements are missing or no items found.');
        return;
    }

    const minSpacing = 30;
    const firstRightOffset = 30;

    let lastBottomLeft = 0;
    let lastBottomRight = 0;

    items?.forEach((item, index) => {
        const card = item.querySelector('.c-timeline-card');
        if (!card) {
            console.warn(`Card not found for item at index ${index}.`);
            return;
        }

        const cardHeight = card.offsetHeight;
        if (!cardHeight) {
            console.warn(`Card height is invalid for item at index ${index}.`);
            return;
        }

        // Determine if the item is on the left or right
        const isLeft = index % 2 === 0;
        item.classList.add(isLeft ? 'c-timeline__item--left' : 'c-timeline__item--right');

        // Calculate the top position for the card
        let top = isLeft
            ? lastBottomLeft + minSpacing
            : lastBottomRight + minSpacing;

        // Apply the offset for the first card on the right
        if (!isLeft && lastBottomRight === 0) {
            top += firstRightOffset;
        }

        // Set the top position of the card
        item.style.top = `${top}px`;

        // Update the last bottom position for the side
        if (isLeft) {
            lastBottomLeft = top + cardHeight;
        } else {
            lastBottomRight = top + cardHeight;
        }
    });

    // Position the timeline line to start at the first dot
    const firstDot = document.querySelector('.c-timeline__dot');
    if (firstDot) {
        const firstDotRect = firstDot.getBoundingClientRect();
        const timelineRect = timeline.getBoundingClientRect();
        const firstDotTop = firstDotRect.top - timelineRect.top;

        // Set the top property of the timeline line
        timelineLine.style.top = `${firstDotTop}px`;
    } else {
        console.warn('First dot not found.');
    }

    // Adjust the height of the timeline container and line
    const maxBottom = Math.max(lastBottomLeft, lastBottomRight);
    if (!isNaN(maxBottom)) {
        timeline.style.height = `${maxBottom + minSpacing}px`;
        timelineLine.style.height = `${maxBottom + minSpacing - parseFloat(timelineLine.style.top)}px`;
    } else {
        console.warn('Invalid maxBottom value. Timeline height not adjusted.');
    }
}

// Run on load and resize (debounced for performance)
document.addEventListener('DOMContentLoaded', setupTimeline);
window.addEventListener('resize', debounce(setupTimeline, 150));

// Debounce helper
function debounce(fn, delay) {
    let timer;
    return function () {
        clearTimeout(timer);
        timer = setTimeout(fn, delay);
    };
}