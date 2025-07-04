{% javascript %}
    const signUpForm = document.getElementById("{{ signUpForm }}");
    const animationTriggerNext = document.getElementById('next-step-btn');
    const animationTriggerBack = document.getElementById('back-step-btn');
    const animationTriggerSkip = document.getElementById('skip-step-btn');
    const saveStayButton = document.getElementById('save_stay');
    const backButton = document.getElementById('back');
    const skipButton = document.getElementById('skip');

    signUpForm?.classList.add('visible');

    animationTriggerNext?.addEventListener('click', () => {
        signUpForm.classList.remove('fade-in', 'visible');
        signUpForm.classList.add('fade-out');
        setTimeout(() => {
            saveStayButton.click();
        }, 500);
    });

    animationTriggerBack?.addEventListener('click', () => {
        signUpForm.classList.remove('fade-in', 'visible');
        signUpForm.classList.add('fade-out-backwards');
        setTimeout(() => {
            backButton.click();
        }, 500);
    });

    animationTriggerSkip?.addEventListener('click', () => {
        signUpForm.classList.remove('fade-in', 'visible');
        signUpForm.classList.add('fade-out');
        setTimeout(() => {
            skipButton.click();
        }, 500);
    });
{% endjavascript %}