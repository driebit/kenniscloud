{% javascript %}
    const signUpForm = document.getElementById("{{ signUpForm }}");
    
    if (signUpForm) {
        signUpForm.classList.add('visible');
    } else {
        console.warn("signUpForm not found");
    }

    const triggers = {
        next: {
            trigger: document.getElementById('next-step-btn'),
            button: document.getElementById('save_stay'),
            animationClass: 'fade-out'
        },
        back: {
            trigger: document.getElementById('back-step-btn'),
            button: document.getElementById('back'),
            animationClass: 'fade-out-backwards'
        },
        skip: {
            trigger: document.getElementById('skip-step-btn'),
            button: document.getElementById('skip'),
            animationClass: 'fade-out'
        }
    };

    const handleTrigger = (animationClass, targetButton) => {
        if (!signUpForm || !targetButton) return;

        signUpForm.classList.remove('fade-in', 'visible');
        signUpForm.classList.add(animationClass);

        setTimeout(() => {
            targetButton.click();
        }, 500);
    }

    Object.values(triggers).forEach(({ trigger, button, animationClass }) => {
        trigger?.addEventListener('click', () => handleTrigger(animationClass, button));
    });

{% endjavascript %}